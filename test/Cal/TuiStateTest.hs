module Cal.TuiStateTest where

import Brick (renderWidget)
import Brick.AttrMap (attrMapLookup)
import qualified Cal.Doc as Doc
import Cal.Journal (journalDataRange)
import Cal.Model.Common (Env (..))
import Cal.Model.Commodities (CommodityRow (..), CommoditiesModel (..), commoditiesModel)
import Cal.Theme (Role (..), nearest256, roleRGB, tintRGB)
import Cal.Tui.App (appStateFor, attrMapFor, bgAttr, blockWidget, calAttrMap, clampRegisterScroll, cursorSpan, draw, pickerBlock, styleAttr)
import Cal.Glyphs (glyphsFor)
import Cal.Model.Month (monthModel)
import Cal.View.Month (monthDoc)
import Cal.View.Types (ViewDoc (..), mkRenderCtx)
import Cal.Tui.State
import Cal.Types
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import qualified Data.Text as T
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

-- | Press keys through 'keyCmd', as the app does.
press :: UiState -> [Vty.Key] -> UiState
press = foldl (\u k -> maybe u (\c -> fst (step t0 c u)) (keyCmd (usPane u) k []))

-- | The picker lists the base (marked default), the other currencies in
-- chip order, then native, with the current choice highlighted.
unit_pickerOpens :: IO ()
unit_pickerOpens = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let entries = pickerEntries (usEnv s0)
  map peValuation entries @?= map ValuedIn ["₦", "$", "Dh", "€", "₹", "KD"] ++ [Native]
  map peDefault entries @?= [True, False, False, False, False, False, False]
  map peLabel entries @?= ["₦ NGN", "$ USD", "Dh AED", "€ EUR", "₹ INR", "KD KWD", "native"]
  usPane (press s0 [Vty.KChar 'c']) @?= PanePicker "" 0 PaneNone
  let euro = s0 { usEnv = (usEnv s0) { envSettings = (envSettings (usEnv s0)) { sValuation = ValuedIn "€" } } }
  usPane (press euro [Vty.KChar 'c']) @?= PanePicker "" 3 PaneNone

-- | Typing filters on code, symbol or label (case-insensitive); ⏎ picks
-- and revalues in place.
unit_pickerFilterPick :: IO ()
unit_pickerFilterPick = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let open = press s0 (map Vty.KChar "cus")
  usPane open @?= PanePicker "us" 0 PaneNone
  map peLabel (pickerMatches "us" (pickerEntries (usEnv s0))) @?= ["$ USD"]
  map peLabel (pickerMatches "€" (pickerEntries (usEnv s0))) @?= ["€ EUR"]
  map peLabel (pickerMatches "NAT" (pickerEntries (usEnv s0))) @?= ["native"]
  let s = press open [Vty.KEnter]
  (usPane s, sValuation (st s), focus s) @?= (PaneNone, ValuedIn "$", d 2026 10 1)

-- | ↑/↓ move within the matches (clamped); Backspace edits the filter,
-- keeping the highlighted entry when it still matches.
unit_pickerMoveBackspace :: IO ()
unit_pickerMoveBackspace = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let s = press s0 [Vty.KChar 'c', Vty.KDown, Vty.KDown, Vty.KUp]
  usPane s @?= PanePicker "" 1 PaneNone
  usPane (press s0 [Vty.KChar 'c', Vty.KUp]) @?= PanePicker "" 0 PaneNone
  usPane (press s0 (Vty.KChar 'c' : replicate 9 Vty.KDown)) @?= PanePicker "" 6 PaneNone
  -- "d" matches $ USD, Dh AED and KD KWD; highlight Dh, then widen
  let f = press s0 [Vty.KChar 'c', Vty.KChar 'd', Vty.KDown]
  usPane f @?= PanePicker "d" 1 PaneNone
  usPane (press f [Vty.KBS]) @?= PanePicker "" 2 PaneNone
  sValuation (st (press f [Vty.KEnter])) @?= ValuedIn "Dh"

-- | Esc cancels with no change; with nothing matching, ⏎ does nothing.
unit_pickerCancelEmpty :: IO ()
unit_pickerCancelEmpty = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let c = press s0 [Vty.KChar 'c', Vty.KDown, Vty.KEsc]
  (usPane c, sValuation (st c)) @?= (PaneNone, ValuedIn "₦")
  let e = press s0 (Vty.KChar 'c' : map Vty.KChar "zzz" ++ [Vty.KEnter])
  pickerMatches "zzz" (pickerEntries (usEnv s0)) @?= []
  (usPane e, sValuation (st e)) @?= (PanePicker "zzz" 0 PaneNone, ValuedIn "₦")
  usPane (press e [Vty.KDown, Vty.KUp]) @?= PanePicker "zzz" 0 PaneNone

-- | While the picker is open, h/j/k/l (and every other letter) go to the
-- filter; only the arrows move.
unit_pickerLettersFilter :: IO ()
unit_pickerLettersFilter = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let s = press s0 (Vty.KChar 'c' : map Vty.KChar "hjklqcv/?[")
  (usPane s, focus s, sValuation (st s), sRateAt (st s)) @?= (PanePicker "hjklqcv/?[" 0 PaneNone, d 2026 10 1, ValuedIn "₦", AtPeriodEnd)
  map (\k -> keyCmd (PanePicker "" 0 PaneNone) (Vty.KChar k) []) "hjkl"
    @?= map (Just . CmdPickerChar) "hjkl"

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

-- | In native mode the picker opens on native, the last entry.
unit_pickerNative :: IO ()
unit_pickerNative = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let n = s0 { usEnv = (usEnv s0) { envSettings = (envSettings (usEnv s0)) { sValuation = Native } } }
  usPane (press n [Vty.KChar 'c']) @?= PanePicker "" 6 PaneNone
  sValuation (st (press n [Vty.KChar 'c', Vty.KUp, Vty.KEnter])) @?= ValuedIn "KD"

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

-- | c opens the picker from the register pane too (spec §10.3); picking
-- or cancelling returns to the pane, scroll kept.
unit_registerPicker :: IO ()
unit_registerPicker = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let r = run s0 [CmdEnter, CmdScroll 2]
      o = press r [Vty.KChar 'c']
      s = press o (map Vty.KChar "us" ++ [Vty.KEnter])
      e = press o [Vty.KDown, Vty.KEsc]
  usPane o @?= PanePicker "" 0 (PaneRegister 2)
  (usPane s, sValuation (st s)) @?= (PaneRegister 2, ValuedIn "$")
  (usPane e, sValuation (st e)) @?= (PaneRegister 2, ValuedIn "₦")

unit_registerFilterReturns :: IO ()
unit_registerFilterReturns = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let r = run s0 [CmdEnter, CmdScroll 2]
      s = press r ([Vty.KChar '/'] ++ map Vty.KChar "acct:fuel" ++ [Vty.KEnter])
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
  cursorSpan 40 (Just 2) Nothing ls @?= Just (1, 3)
  cursorSpan 40 (Just 4) Nothing ls @?= Just (4, 4)
  cursorSpan 40 (Just 0) Nothing [plainL] @?= Just (0, 0)
  cursorSpan 40 Nothing Nothing ls @?= Nothing
  -- a box taller than the viewport (a long Week day column): the cursor
  -- row through the end of the selected item, at most a viewport tall
  let tall = replicate 30 boxL
  cursorSpan 10 (Just 12) (Just 15) tall @?= Just (12, 15)
  cursorSpan 10 (Just 12) Nothing tall @?= Just (12, 12)
  cursorSpan 3 (Just 12) (Just 15) tall @?= Just (12, 14)
  cursorSpan 30 (Just 12) (Just 15) tall @?= Just (0, 29)

-- | Spec §10.5 in the Week grid: on a screen too short for a long day
-- column, ↓ walks the selection down the day and the viewport follows it.
unit_weekViewportFollows :: IO ()
unit_weekViewportFollows = do
  env <- envFor ["-W", "-p", "2026-10-14", "-f", "test/fixtures/week-busy.journal"]
  let screen us = map (reverse . dropWhile (== ' ') . reverse)
        (concatMap imageRows (take 1 (Vty.picLayers
          (renderWidget (Just calAttrMap) (draw (appStateFor us t0)) (140, 45)))))
      onScreen name us = any (\r -> (name ++ " ") `isInfixOf` r || (name ++ "│") `isInfixOf` r) (screen us)
      s0 = (initState env) { usWidth = 140 }
  assertBool "first errand on screen" (onScreen "Errand 1" s0)
  assertBool "the last errand is below the viewport" (not (onScreen "Errand 12" s0))
  let walk = scanl (\u c -> fst (step t0 c u)) s0 (replicate 11 (CmdMove 0 1))
  mapM_ (\(i, u) -> assertBool ("Errand " ++ show i ++ " follows the selection")
                      (onScreen ("Errand " ++ show i) u))
        (zip [1 :: Int ..] walk)
  assertEqual "the selection reached the last errand" 11 (cuWeekTxn (usCursor (last walk)))

-- | The rows of a rendered vty image as (character, attribute) cells.
imageCells :: Image -> [[(Char, Vty.Attr)]]
imageCells img = case img of
  HorizText a t _ _ -> [[ (c, a) | c <- TL.unpack t ]]
  HorizJoin l r _ h -> zipWith (++) (padRows h (imageCells l)) (padRows h (imageCells r))
  VertJoin a b _ _ -> imageCells a ++ imageCells b
  BGFill w h -> replicate h (replicate w (' ', Vty.defAttr))
  Crop i left top w h -> take h [ take w (drop left r) | r <- drop top (imageCells i) ]
  EmptyImage -> []
  where
    padRows h rs = take h (rs ++ repeat [])

-- | Design F5 fix: in the TUI the Week day columns run down to the footer.
-- At 140×60 on week 42 (cursor on Fri 16) the grid's bottom rule is the
-- last body row, one blank row above the footer, and the cursor column's
-- edges are unbroken from the top rule down to it.
unit_weekColumnsToFooter :: IO ()
unit_weekColumnsToFooter = do
  env <- envFor ["-W", "-p", "2026-10-16"]
  let pic = renderWidget (Just calAttrMap) (draw (appStateFor ((initState env) { usWidth = 140 }) t0)) (140, 60)
      cells = concatMap imageCells (take 1 (Vty.picLayers pic))
      rows = map (map fst) cells
      cursorAttr = attrMapLookup (styleAttr Doc.cursorBorder) calAttrMap
      gridTop = [ i | (i, r) <- zip [0 :: Int ..] rows, take 1 r == "┌", '┬' `elem` r, "Mon" `isInfixOf` (rows !! (i + 1)) ]
      bottoms = [ i | (i, r) <- zip [0 :: Int ..] rows, take 1 r == "└", '┴' `elem` r ]
  assertEqual "screen rows" 60 (length rows)
  top <- case gridTop of
    (x : _) -> pure x
    [] -> assertFailure ("no grid top: " ++ unlines rows)
  bottom <- case filter (> top) bottoms of
    (x : _) -> pure x
    [] -> assertFailure "no grid bottom"
  assertEqual "blank row under the grid" "" (dropWhileEnd' (rows !! (bottom + 1)))
  -- everything under that blank row is the footer: the key hints (two
  -- rows at 140) down to the screen's last row, no filler rows
  let footer = drop (bottom + 2) rows
  assertBool ("the footer follows: " ++ show footer) ("transaction" `isInfixOf` concat footer && "quit" `isInfixOf` concat footer)
  assertBool ("no filler between the grid and the footer: " ++ show footer)
    (length footer <= 2 && all (not . null . dropWhileEnd') footer)
  -- the cursor column: its edges on the top rule
  let edgeCols = [ c | (c, (_, a)) <- zip [0 :: Int ..] (cells !! top), a == cursorAttr ]
  assertBool ("cursor edges on the top rule: " ++ show edgeCols) (length edgeCols > 2)
  let c0 = minimum edgeCols
      c1 = maximum edgeCols
  mapM_ (\i -> assertBool ("cursor edges on row " ++ show i)
                 (snd (cells !! i !! c0) == cursorAttr && snd (cells !! i !! c1) == cursorAttr))
        [top .. bottom]
  where
    dropWhileEnd' = reverse . dropWhile (== ' ') . reverse

-- | The rows of a rendered vty image as plain text (one entry per
-- character; the fixture's wide characters don't occur in the rows used).
imageRows :: Image -> [String]
imageRows img = case img of
  HorizText _ t _ _ -> [TL.unpack t]
  HorizJoin l r _ h -> zipWith (++) (padRows h (imageRows l)) (padRows h (imageRows r))
  VertJoin a b _ _ -> imageRows a ++ imageRows b
  BGFill w h -> replicate h (replicate w ' ')
  Crop i left top w h -> take h [ take w (drop left r) | r <- drop top (imageRows i) ]
  EmptyImage -> []
  where
    padRows h rs = take h (rs ++ repeat "")

-- | Design S3: on the real TUI screen, the first Month panel row sits
-- exactly one blank row under the last body row, whether the body fits the
-- screen (200×80) or scrolls in a shorter viewport (200×40).
unit_panelPlacement :: IO ()
unit_panelPlacement = do
  env <- envFor ["-M", "-p", "2026-10-30"]
  ctx <- maybe (assertFailure "no render context") pure (mkRenderCtx env 200 True)
  let doc = monthDoc ctx (monthModel env)
      bodyLs = T.lines (Doc.plainText (vdBody doc))
      topN = length (Doc.bLines (vdTop doc))
      strip = reverse . dropWhile (== ' ') . reverse
      check h = do
        let pic = renderWidget (Just calAttrMap) (draw (appStateFor (initState env) t0)) (200, h)
            rows = map strip (concatMap imageRows (take 1 (Vty.picLayers pic)))
            -- the detail and rates panels' top borders, side by side
            isPanelTop r = "┐  ┌" `isInfixOf` r
            panelTops = [ i | (i, r) <- zip [0 :: Int ..] rows, i > topN, isPanelTop r ]
            isBody r = T.pack r `elem` map (T.pack . strip . T.unpack) bodyLs
        p <- case panelTops of
          (x : _) -> pure x
          [] -> assertFailure ("no panel row at height " ++ show h ++ ": " ++ show rows)
        assertEqual ("blank row above the panels at height " ++ show h) "" (rows !! (p - 1))
        assertBool ("a body row right above the blank row at height " ++ show h ++ ": " ++ show (rows !! (p - 2))) (isBody (rows !! (p - 2)))
        pure (p, rows)
  (pTall, rowsTall) <- check 80
  -- the whole body fits: its last line is the grid's bottom edge
  assertEqual "the body ends right above" (T.unpack (T.stripEnd (last bodyLs))) (rowsTall !! (pTall - 2))
  assertEqual "the body starts one blank row under the top" (pTall - 2 - length bodyLs) topN
  (pShort, _) <- check 40
  assertBool "the short screen scrolls the body" (pShort - 2 - topN < length bodyLs)

-- The Commodities cursor opens on the first non-base row (review M8); moving
-- and "value in this" start from there.
unit_commInitialCursor :: IO ()
unit_commInitialCursor = do
  c <- mk ["--commodities", "-p", "2026-10"]
  let sel u = let m = commoditiesModel (usEnv u) (usCursor u) in fmap crCommodity (take 1 (drop (cmSelected m) (cmCurrencies m ++ cmSecurities m)))
  sel c @?= ["$"]
  sel (run c [CmdMove 0 1]) @?= ["Dh"]
  sel (run c [CmdMove 0 (-1)]) @?= ["\8358"]
  sValuation (st (run c [CmdEnter])) @?= ValuedIn "$"

-- | 256-colour mode uses the nearest xterm index for every role and tint,
-- foreground and background: the panel tint #13171c is 234 (#1c1c1c, at
-- squared distance 106; 233 #121212 is at 126), the bar track 236.
unit_colour256 :: IO ()
unit_colour256 = do
  let m = attrMapFor Color256
      idx n = Vty.SetTo (Vty.Color240 (fromIntegral (n - 16 :: Int)))
  nearest256 (0x13, 0x17, 0x1c) @?= 234
  nearest256 (0x2a, 0x2f, 0x37) @?= 236
  Vty.attrBackColor (attrMapLookup (bgAttr RPanel 100) m) @?= idx 234
  Vty.attrForeColor (attrMapLookup (styleAttr (Doc.st RRule)) m) @?= idx 236
  mapM_ (\r -> mapM_ (\t -> do
      let c = nearest256 (tintRGB t (roleRGB r))
      Vty.attrBackColor (attrMapLookup (bgAttr r t) m) @?= idx c
      if r == RPanel then pure ()
      else Vty.attrForeColor (attrMapLookup (styleAttr (Doc.tinted t (Doc.st r))) m) @?= idx c)
    [100, 60, 40, 25]) [minBound .. maxBound :: Role]

-- | A date: term typed into the / filter sets the focus period, as on the
-- command line; the other terms filter.
unit_filterDateSetsPeriod :: IO ()
unit_filterDateSetsPeriod = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let s = run s0 (CmdFilterOpen : map CmdFilterChar "date:2026-08 acct:fuel" ++ [CmdFilterSubmit])
  (focus s, sQueryArgs (st s)) @?= (d 2026 8 1, ["acct:fuel"])
  let w = run s0 [CmdFace FaceWeek]
      s' = run w (CmdFilterOpen : map CmdFilterChar "date:2026-10-14" ++ [CmdFilterSubmit])
  (sFace (st s'), focus s', sQueryArgs (st s'), sQuery (st s')) @?= (FaceWeek, d 2026 10 14, [], Any)

-- | The picker box: titled "value in", the prompt, the matches with the
-- highlighted one in reverse video and the base tagged default, the key
-- hint; a plain (rule) border, never the cursor colour.
unit_pickerBlock :: IO ()
unit_pickerBlock = do
  s0 <- mk ["-M", "-p", "2026-10"]
  let entries = pickerEntries (usEnv s0)
      b = pickerBlock (glyphsFor GlyphsUtf8) "▏" entries "" 1
      ls = T.lines (Doc.plainText b)
      cells = concat [ [ (c, sty) | c <- T.unpack t ] | Doc.Line ss <- Doc.bLines b, Doc.Seg sty t <- ss ]
      border = [ sty | (c, sty) <- cells, c `elem` ("┌┐└┘│─" :: String) ]
      styleOf needle = [ sty | Doc.Line ss <- Doc.bLines b, Doc.Seg sty t <- ss, needle `T.isInfixOf` t ]
  assertBool (show ls) ("┌─value in" `T.isPrefixOf` (ls !! 0))
  assertBool (show ls) ("│ ▸ ▏" `T.isPrefixOf` (ls !! 1))
  assertBool (show ls) ("₦ NGN" `T.isInfixOf` (ls !! 3) && "default │" `T.isSuffixOf` (ls !! 3))
  assertBool (show ls) ("↑↓ ⏎ pick · esc" `T.isInfixOf` (ls !! (length ls - 2)))
  assertBool "every line one width" (all ((== Doc.bWidth b) . Doc.textW) ls)
  map Doc.stInverse (styleOf "$ USD") @?= [True]
  map Doc.stInverse (styleOf "₦ NGN") @?= [False]
  map Doc.stRole (styleOf "default") @?= [RDim]
  assertBool "border in the rule role" (not (null border) && all ((== RRule) . Doc.stRole) border)
  -- filtered: the box keeps its size
  let nb = pickerBlock (glyphsFor GlyphsUtf8) "▏" entries "us" 0
  (Doc.bWidth nb, length (Doc.bLines nb)) @?= (Doc.bWidth b, length (Doc.bLines b))
  -- ASCII: > prompt, ASCII box and key names
  let a = T.lines (Doc.plainText (pickerBlock (glyphsFor GlyphsAscii) "_" entries "us" 0))
  assertBool (show a) ("+-value in" `T.isPrefixOf` (a !! 0) && "| > us_" `T.isPrefixOf` (a !! 1))
  assertBool (show a) ("up/down enter pick . esc" `T.isInfixOf` (a !! (length a - 2)))

-- | On screen at 140×45 the picker is drawn over the view, and over the
-- register pane when opened from there.
unit_pickerOnScreen :: IO ()
unit_pickerOnScreen = do
  s0 <- mk ["-M", "-p", "2026-10-30"]
  let screen us = concatMap imageRows (Vty.picLayers (renderWidget (Just calAttrMap) (draw (appStateFor us t0)) (140, 45)))
      has t us = any (t `isInfixOf`) (screen us)
      o = press s0 (map Vty.KChar "cus")
  assertBool "picker drawn" (has "value in" o && has "▸ us" o && has "$ USD" o)
  assertBool "no picker once picked" (not (has "value in" (press o [Vty.KEnter])))
  let r = press s0 [Vty.KEnter, Vty.KChar 'c']
  assertBool "picker over the register" (has "value in" r && has "Register" r)

-- | The screen as seen: the picture's layers composited, top first, each
-- layer's background fill letting the layers under it show through. A wide
-- character takes its cell and a '\0' one after it.
screenGrid :: Vty.Picture -> [String]
screenGrid pic = foldr1 over (map cells (Vty.picLayers pic))
  where
    over top below = [ zipWith pick (pad (length b) t) b | (t, b) <- zip (top ++ repeat []) below ]
    pick c b = if c == '\1' then b else c
    pad n r = take n (r ++ repeat '\1')
    cells img = case img of
      HorizText _ t _ _ -> [concat [ if Doc.textW (T.singleton c) == 2 then [c, '\0'] else [c] | c <- TL.unpack t ]]
      HorizJoin l r _ h -> zipWith (\a b -> pad (Vty.imageWidth l) a ++ b) (rows h (cells l)) (rows h (cells r))
      VertJoin a b _ _ -> cells a ++ cells b
      BGFill w h -> replicate h (replicate w '\1')
      Crop i left top w h -> take h [ pad w (take w (drop left r)) | r <- drop top (cells i) ]
      EmptyImage -> []
    rows h rs = take h (rs ++ repeat [])

-- | The cells of the one-cell ring around the box whose top border holds
-- the label: the row above, the row below and the columns either side.
ringAround :: String -> [String] -> Maybe [Char]
ringAround label grid = do
  (top, at) <- only [ (r, c) | (r, row) <- zip [0 ..] grid, c <- take 1 (indicesOf row) ]
  let row = grid !! top
      cell r c = (grid !! r) !! c
  left <- lastMay [ i | i <- [0 .. at], row !! i == '┌' ]
  right <- firstOf [ i | i <- [at .. length row - 1], row !! i == '┐' ]
  bottom <- firstOf [ r | r <- [top + 1 .. length grid - 1], cell r left == '└' ]
  pure ( [ cell r c | r <- [top - 1, bottom + 1], c <- [left - 1 .. right + 1] ]
           ++ [ cell r c | r <- [top .. bottom], c <- [left - 1, right + 1] ] )
  where
    indicesOf row = [ i | i <- [0 .. length row - length label], take (length label) (drop i row) == label ]
    only xs = case xs of { [x] -> Just x; _ -> Nothing }
    firstOf xs = case xs of { (x : _) -> Just x; [] -> Nothing }
    lastMay xs = firstOf (reverse xs)

-- | The picker and the help box are each set off from the view under them
-- by a blank one-cell ring.
unit_overlayRing :: IO ()
unit_overlayRing = do
  s0 <- (\u -> u { usWidth = 140 }) <$> mk ["-M", "-p", "2026-10-30"]
  let grid us = screenGrid (renderWidget (Just calAttrMap) (draw (appStateFor us t0)) (140, 45))
      picker = grid (press s0 [Vty.KChar 'c'])
      help = grid (press s0 [Vty.KChar '?'])
      blank = fmap (all (== ' '))
  assertEqual (unlines picker) (Just True) (blank (ringAround "value in" picker))
  assertEqual (unlines help) (Just True) (blank (ringAround "keys" help))
