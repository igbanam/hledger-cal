module Cal.TuiStateTest where

import Brick (renderWidget)
import Brick.AttrMap (attrMapLookup)
import qualified Cal.Doc as Doc
import Cal.Journal (journalDataRange)
import Cal.Model.Common (Env (..))
import Cal.Model.Commodities (CommodityRow (..), CommoditiesModel (..), commoditiesModel)
import Cal.Theme (Role (..))
import Cal.Tui.App (ScreenRows (..), attrMapFor, bgAttr, blockWidget, calAttrMap, clampRegisterScroll, cursorSpan, screenRows, styleAttr)
import Cal.Model.Month (monthModel)
import Cal.View.Month (monthDoc)
import Cal.View.Types (ViewDoc (..), mkRenderCtx)
import Cal.Tui.State
import Cal.Types
import Data.Maybe (isJust)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import qualified Data.Text.Lazy as TL
import qualified Graphics.Vty as Vty
import Graphics.Vty.Image.Internal (Image (..))
import Hledger.Query (Query (Any))
import Support (edgePath, envFor, fixtureToday, loadFixture)
import Test.Tasty.HUnit

d :: Integer -> Int -> Int -> Day
d = fromGregorian

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 11 6) 0

focus :: UiState -> Day
focus = sFocus . envSettings . usEnv

st :: UiState -> Settings
st = envSettings . usEnv

run :: UiState -> [Cmd] -> UiState
run = foldl (\s c -> fst (step t0 c s))

mk :: [String] -> IO UiState
mk args = initState <$> envFor args

unit_moveDay, unit_moveWeek, unit_moveIntoNext, unit_nextPeriod, unit_clampFuture :: IO ()
unit_moveDay = (\s0 -> focus (run s0 [CmdMove 1 0]) @?= d 2026 10 2) =<< mk ["-M", "-p", "2026-10"]
unit_moveWeek = (\s0 -> focus (run s0 [CmdMove 0 1]) @?= d 2026 10 8) =<< mk ["-M", "-p", "2026-10"]
unit_moveIntoNext = (\s0 -> focus (run s0 [CmdMove 0 (-1)]) @?= d 2026 9 24) =<< mk ["-M", "-p", "2026-10"]
unit_nextPeriod = (\s0 -> focus (run s0 [CmdPeriod 1]) @?= d 2026 11 1) =<< mk ["-M", "-p", "2026-10"]
unit_clampFuture = (\s0 -> focus (run s0 (replicate 3 (CmdPeriod 1))) @?= fixtureToday) =<< mk ["-M", "-p", "2026-10"]

unit_faceKeepsDate :: IO ()
unit_faceKeepsDate = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let s = run s0 [CmdMove 0 3, CmdFace FaceWeek]
  (sFace (st s), focus s) @?= (FaceWeek, d 2026 10 22)

unit_cycleCurrency :: IO ()
unit_cycleCurrency = do
  s0 <- mk ["-M", "-p", "2026-10"]
  map (sValuation . st) (scanl (\s c -> fst (step t0 c s)) s0 (replicate 7 CmdCycleCurrency))
    @?= map ValuedIn ["₦", "$", "Dh", "€", "₹", "KD"] ++ [Native, ValuedIn "₦"]

unit_cycleRate :: IO ()
unit_cycleRate = do
  s0 <- mk ["-M", "-p", "2026-10"]
  sRateAt (st (run s0 [CmdCycleRate, CmdCycleRate])) @?= AtToday

unit_enterRegister, unit_escCloses :: IO ()
unit_enterRegister = (\s0 -> usPane (run s0 [CmdEnter]) @?= PaneRegister 0) =<< mk ["-M", "-p", "2026-10"]
unit_escCloses = (\s0 -> usPane (run s0 [CmdEnter, CmdEsc]) @?= PaneNone) =<< mk ["-M", "-p", "2026-10"]

unit_drillAndBack :: IO ()
unit_drillAndBack = do
  y <- mk ["-Y"]
  let s = run y [CmdMove 0 (-1), CmdEnter]
  (sFace (st s), focus s) @?= (FaceMonth, d 2026 10 1)
  let b = fst (step t0 CmdEsc s)
  (sFace (st b), focus b) @?= (FaceYear, d 2026 10 6)

unit_filterOk, unit_filterBad :: IO ()
unit_filterOk = do
  s0 <- mk ["-M", "-p", "2026-10"]
  sQueryArgs (st (run s0 (CmdFilterOpen : map CmdFilterChar "acct:fuel" ++ [CmdFilterSubmit]))) @?= ["acct:fuel"]
unit_filterBad = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let s = run s0 (CmdFilterOpen : map CmdFilterChar "date:nonsense" ++ [CmdFilterSubmit])
  (sQueryArgs (st s), isJust (usMessage s)) @?= ([], True)

unit_quit :: IO ()
unit_quit = (\s0 -> snd (step t0 CmdQuit s0) @?= EffQuit) =<< mk ["-M", "-p", "2026-10"]

unit_reloadError, unit_messageExpires :: IO ()
unit_reloadError = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let s = applyReload t0 (Left "parse error at line 3\nmore") s0
  (visibleMessage t0 s, focus s) @?= (Just "parse error at line 3", d 2026 10 1)
unit_messageExpires = do
  s0 <- mk ["-M", "-p", "2026-10"]
  visibleMessage (addUTCTime 4 t0) (applyReload t0 (Left "x") s0) @?= Nothing

unit_commNoPrice :: IO ()
unit_commNoPrice = do
  c <- mk ["--commodities", "-p", "2026-10", "-f", edgePath]
  let s = run c [CmdMove 0 1, CmdEnter]
  fmap fst (usMessage s) @?= Just "no price for BTC"

unit_keys :: IO ()
unit_keys =
  (keyCmd PaneNone (Vty.KChar 'Y') [], keyCmd PaneNone (Vty.KChar ']') [], keyCmd (PaneFilter "" PaneNone) (Vty.KChar 'q') [])
    @?= (Just (CmdFace FaceYear), Just (CmdPeriod 1), Just (CmdFilterChar 'q'))

-- Native-mode "value in this" (controller ruling).
unit_commNativePriced :: IO ()
unit_commNativePriced = do
  c <- mk ["--commodities", "-p", "2026-10"]
  let rowsOf u = let m = commoditiesModel (usEnv u) (usCursor u) in cmCurrencies m ++ cmSecurities m
      n = c { usEnv = (usEnv c) { envSettings = (envSettings (usEnv c)) { sValuation = Native } } }
      priced = [crCommodity r | r <- rowsOf c, not (crIsBase r), crPrice r /= Nothing]
      idx = [i | (i, r) <- zip [0 ..] (rowsOf n), crCommodity r `elem` take 1 priced]
  case (idx, priced) of
    (i : _, p : _) -> do
      let s = run n [CmdMove 0 i, CmdEnter]
      (usMessage s, sValuation (st s)) @?= (Nothing, ValuedIn p)
    _ -> assertFailure "fixture has no priced non-base row"

unit_commNativeUnpriced :: IO ()
unit_commNativeUnpriced = do
  c <- mk ["--commodities", "-p", "2026-10", "-f", edgePath]
  let n = c { usEnv = (usEnv c) { envSettings = (envSettings (usEnv c)) { sValuation = Native } } }
      s = run n [CmdMove 0 1, CmdEnter]
  (fmap fst (usMessage s), sValuation (st s)) @?= (Just "no price for BTC", Native)

unit_filterEmptyClears :: IO ()
unit_filterEmptyClears = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let s = run s0 (CmdFilterOpen : map CmdFilterChar "acct:fuel" ++ [CmdFilterSubmit, CmdFilterOpen] ++ replicate 9 CmdFilterBackspace ++ [CmdFilterSubmit])
  (sQueryArgs (st s), sQuery (st s), usPane s) @?= ([], Any, PaneNone)

unit_reloadRecomputesRange :: IO ()
unit_reloadRecomputesRange = do
  s0 <- mk ["-M", "-p", "2026-10"]
  j <- loadFixture edgePath
  let s = applyReload t0 (Right j) s0
  sDataRange (st s) @?= journalDataRange (sToday (st s)) j

unit_nativeCycleToFirst :: IO ()
unit_nativeCycleToFirst = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let n = s0 { usEnv = (usEnv s0) { envSettings = (envSettings (usEnv s0)) { sValuation = Native } } }
  sValuation (st (run n [CmdCycleCurrency])) @?= ValuedIn "₦"

unit_escNothing :: IO ()
unit_escNothing = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let s = run s0 [CmdEsc]
  (usPane s, usBack s, focus s) @?= (PaneNone, [], d 2026 10 1)

-- | Attrs of every text run containing the given text in a rendered image.
attrsOf :: TL.Text -> Image -> [Vty.Attr]
attrsOf t img = case img of
  HorizText a txt _ _ -> [a | t `TL.isInfixOf` txt]
  HorizJoin l r _ _ -> attrsOf t l ++ attrsOf t r
  VertJoin a b _ _ -> attrsOf t a ++ attrsOf t b
  Crop i _ _ _ _ -> attrsOf t i
  _ -> []

unit_attrMapCoversRoles :: IO ()
unit_attrMapCoversRoles = do
  let style = Doc.bold (Doc.tinted 60 (Doc.st RExp))
      pic = renderWidget (Just calAttrMap) [blockWidget (Doc.str style "x")] (10, 1)
      expected = attrMapLookup (styleAttr style) calAttrMap
  assertBool "attr name is defined in calAttrMap" (expected /= Vty.defAttr)
  concatMap (attrsOf "x") (Vty.picLayers pic) @?= [expected]

unit_registerCycleCurrency :: IO ()
unit_registerCycleCurrency = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let r = run s0 [CmdEnter]
  cmd <- maybe (assertFailure "c unmapped in register") pure (keyCmd (usPane r) (Vty.KChar 'c') [])
  let s = run r [cmd]
  (usPane r, usPane s, sValuation (st s) /= sValuation (st r)) @?= (PaneRegister 0, PaneRegister 0, True)

unit_registerFilterReturns :: IO ()
unit_registerFilterReturns = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let r = run s0 [CmdEnter, CmdScroll 2]
      press u k = maybe u (\c -> fst (step t0 c u)) (keyCmd (usPane u) k [])
      s = foldl press r ([Vty.KChar '/'] ++ map Vty.KChar "acct:fuel" ++ [Vty.KEnter])
      e = run r [CmdFilterOpen, CmdFilterChar 'x', CmdEsc]
  (usPane s, sQueryArgs (st s), usPane e) @?= (PaneRegister 0, ["acct:fuel"], PaneRegister 2)

unit_reloadErrorKeepsParseText :: IO ()
unit_reloadErrorKeepsParseText = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let err = "/tmp/x/j.journal:284:1:\n    |\n284 | bad line\n    | ^\nunexpected 't'\nexpecting end of input\n"
      s = applyReload t0 (Left err) s0
  fmap fst (usMessage s) @?= Just "j.journal:284:1: unexpected 't'"
  focus s @?= focus s0

unit_clampRegisterScroll :: IO ()
unit_clampRegisterScroll =
  -- height 10 shows 7 rows; 12 rows can scroll by at most 5
  (clampRegisterScroll 10 12 1000000, clampRegisterScroll 10 12 3, clampRegisterScroll 10 4 9, clampRegisterScroll 10 12 (-2))
    @?= (5, 3, 0, 0)

-- | Every role (the cursor border included) has an attribute at every tint.
unit_attrMapEveryRole :: IO ()
unit_attrMapEveryRole =
  mapM_ (\r -> mapM_ (\t -> do
      let a = attrMapLookup (styleAttr (Doc.tinted t (Doc.st r))) calAttrMap
      assertBool (show (r, t)) (a /= Vty.defAttr)
      assertBool (show (r, t) ++ " bg") (attrMapLookup (bgAttr r t) calAttrMap /= Vty.defAttr))
    [100, 60, 40, 25]) [minBound .. maxBound :: Role]

-- | A style with a background renders with both colours.
unit_attrBackground :: IO ()
unit_attrBackground = do
  let style = Doc.onBg RExp 100 (Doc.st RInc)
      pic = renderWidget (Just calAttrMap) [blockWidget (Doc.str style "x")] (10, 1)
  case concatMap (attrsOf "x") (Vty.picLayers pic) of
    [a] -> (Vty.attrForeColor a, Vty.attrBackColor a)
      @?= (Vty.attrForeColor (attrMapLookup (styleAttr (Doc.st RInc)) calAttrMap), Vty.SetTo (Vty.rgbColor (244 :: Int) 112 103))
    as -> assertFailure (show as)

-- | Without colour the cursor border is bold, and nothing else changes.
unit_cursorBorderNoColour :: IO ()
unit_cursorBorderNoColour = do
  let a = attrMapLookup (styleAttr Doc.cursorBorder) (attrMapFor ColorNone)
  (Vty.attrStyle a, Vty.attrForeColor a) @?= (Vty.SetTo Vty.bold, Vty.Default)

-- | Year: with the ≥ 140 grid, ↑/↓ move a quarter row (±3 months) and ←/→
-- one month; narrower, the rows list keeps ↑/↓ ±1 and ←/→ ±3.
unit_yearGridMoves :: IO ()
unit_yearGridMoves = do
  y <- mk ["-Y"]
  let grid = y { usWidth = 140 }
  map focus [run grid [CmdMove 0 (-1)], run grid [CmdMove (-1) 0]] @?= [d 2026 8 6, d 2026 10 6]
  let rows = y { usWidth = 139 }
  map focus [run rows [CmdMove 0 (-1)], run rows [CmdMove (-1) 0]] @?= [d 2026 10 6, d 2026 8 6]
  usWidth y @?= 0

-- | In the ≥ 140 Year grid, ↑/↓ that would leave the data range does
-- nothing (it used to clamp, moving the cursor diagonally).
unit_yearGridEdges :: IO ()
unit_yearGridEdges = do
  y <- mk ["-Y"]
  let at day = y { usWidth = 140, usEnv = (usEnv y) { envSettings = (envSettings (usEnv y)) { sFocus = day } } }
      up s0 = focus (run s0 [CmdMove 0 (-1)])
      down s0 = focus (run s0 [CmdMove 0 1])
  up (at (d 2026 1 15)) @?= d 2026 1 15
  up (at (d 2026 2 10)) @?= d 2026 2 10
  down (at (d 2026 9 6)) @?= d 2026 9 6
  down (at (d 2026 8 6)) @?= d 2026 11 6

-- | A grid step into the previous year moves when that month lies in the
-- data range: the rule is the range, not the year shown.
unit_yearGridCrossYear :: IO ()
unit_yearGridCrossYear = do
  y <- mk ["-Y"]
  let at day = y { usWidth = 140, usEnv = (usEnv y) { envSettings = (envSettings (usEnv y))
                   { sFocus = day, sDataRange = (d 2025 3 10, d 2026 11 6) } } }
      up s0 = focus (run s0 [CmdMove 0 (-1)])
  up (at (d 2026 2 10)) @?= d 2025 11 10
  up (at (d 2026 1 15)) @?= d 2025 10 15
  up (at (d 2025 5 20)) @?= d 2025 5 20
  up (at (d 2025 6 2)) @?= d 2025 3 10

-- | The viewport keeps the whole cursor box in view: the cursor row and
-- the lines around it that hold the cursor border.
unit_cursorSpan :: IO ()
unit_cursorSpan = do
  let plainL = Doc.Line [Doc.Seg (Doc.st RText) "x"]
      boxL = Doc.Line [Doc.Seg (Doc.st RText) "x", Doc.Seg Doc.cursorBorder "│"]
      ls = [plainL, boxL, boxL, boxL, plainL, boxL]
  cursorSpan (Just 2) ls @?= Just (1, 3)
  cursorSpan (Just 4) ls @?= Just (4, 4)
  cursorSpan (Just 0) [plainL] @?= Just (0, 0)
  cursorSpan Nothing ls @?= Nothing

-- | Design S3: the Month panels start one blank row under the last body
-- line, whether the body fits the screen or scrolls in a shorter viewport.
unit_panelPlacement :: IO ()
unit_panelPlacement = do
  env <- envFor ["-M", "-p", "2026-10-30"]
  ctx <- maybe (assertFailure "no render context") pure (mkRenderCtx env 200 True)
  let doc = monthDoc ctx (monthModel env)
      n f = length (Doc.bLines (f doc))
      (topN, bodyN, panelN) = (n vdTop, n vdBody, n vdBottom)
      tall = screenRows 80 topN bodyN panelN 2
      short = screenRows 40 topN bodyN panelN 2
  srBodyH tall @?= bodyN
  srPanelRow tall @?= topN + 1 + bodyN + 1
  assertBool "the short screen scrolls the body" (srBodyH short < bodyN)
  srPanelRow short @?= topN + 1 + srBodyH short + 1
  -- the panels and the footer still fit under the short body
  assertBool "panels fit" (srPanelRow short + panelN + 1 + 2 <= 40)
