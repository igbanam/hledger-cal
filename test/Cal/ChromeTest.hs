module Cal.ChromeTest where

import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit

import Cal.Doc
import Cal.Model.Common (Env (..), Mix (..), kpisFor)
import Cal.Model.Shared (currencyBar)
import Cal.Period (intervalSpan)
import Cal.Theme (Role (..))
import Cal.Types
import Cal.View.Chrome
import Cal.View.Types
import Support (envFor)

-- | The month chrome for October 2026 at width @w@.
monthChrome :: [String] -> Int -> IO Block
monthChrome extra w = do
  env <- envFor (["-M", "-p", "2026-10"] ++ extra)
  let ctx = fromJust (mkRenderCtx env w False)
      sp = intervalSpan IMonth (sFocus (envSettings env))
      k = kpisFor env IMonth sp
      cards = cardRow ctx
        [ \cw -> incomeCard ctx cw "Sep" k
        , \cw -> expensesCard ctx cw "Sep" k
        , \cw -> netCard ctx cw k
        , \cw -> fxCard ctx cw k
        , \cw -> activityCard ctx cw k ]
  pure (topChrome ctx "October 2026" (currencyBar env sp) [cards])

-- | (name, extra args, width, renderer)
variants :: [(String, [String], Int, Block -> Text)]
variants =
  [ ("chrome-month-" ++ show w, [], w, plainText) | w <- [140, 120, 100, 80] ]
  ++ [ ("chrome-month-usd-140", ["-X", "$"], 140, plainText)
     , ("chrome-month-140-ansi", [], 140, renderAnsi ColorTrue)
     , ("chrome-month-140-ascii", ["--ascii"], 140, plainText) ]

utf8 :: Text -> BL.ByteString
utf8 t = BL.fromStrict (TE.encodeUtf8 (t <> "\n"))

test_goldens :: TestTree
test_goldens = testGroup "goldens"
  [ goldenVsString name ("test/golden/" ++ name ++ ".txt") (utf8 . render <$> monthChrome extra w)
  | (name, extra, w, render) <- variants ]

unit_widths :: Assertion
unit_widths = mapM_ check variants
  where
    check (name, extra, w, _) = do
      b <- monthChrome extra w
      let wide = [ l | l <- T.lines (plainText b), textW l > w ]
      assertBool (name ++ " has lines wider than " ++ show w ++ ": " ++ show wide) (null wide)
      assertBool (name ++ " reports width " ++ show (bWidth b)) (bWidth b <= w)

unit_tiers :: Assertion
unit_tiers = map tierFor [79, 80, 99, 100, 119, 120, 139, 140]
  @?= [Nothing, Just Tier80, Just Tier80, Just Tier100, Just Tier100, Just Tier120, Just Tier120, Just TierFull]

unit_noCtxBelow80 :: Assertion
unit_noCtxBelow80 = do
  env <- envFor ["-M", "-p", "2026-10"]
  assertBool "width 79 has no context" (null (mkRenderCtx env 79 False))

unit_footerMessage :: Assertion
unit_footerMessage = do
  env <- envFor ["-M", "-p", "2026-10"]
  let ctx = (fromJust (mkRenderCtx env 80 True)) { rcMessage = Just "no price for X" }
      t = plainText (footerBlock ctx [("q", "quit")] (str (st RDim) "legend"))
  assertBool (show t) ("no price for X" `T.isInfixOf` t && not ("quit" `T.isInfixOf` t))
  assertBool (show t) ("legend" `T.isSuffixOf` t)

ctxWith :: [String] -> Int -> IO RenderCtx
ctxWith extra w = do
  env <- envFor (["-M", "-p", "2026-10"] ++ extra)
  pure (fromJust (mkRenderCtx env w False))

segText :: [Seg] -> Text
segText ss = T.concat [ t | Seg _ t <- ss ]

-- | The KPI strip is one bordered box: top and bottom borders, sides, and
-- a divider between cards, joined to the borders (design S1).
unit_kpiStripBordered :: Assertion
unit_kpiStripBordered = do
  b <- monthChrome [] 140
  -- the strip follows the currency bar's box (its first 6 lines)
  (top, strip) <- case dropWhile (not . ("┌" `T.isPrefixOf`)) (drop 6 (T.lines (plainText b))) of
    t : rest -> pure (t, takeWhile (not . T.null) rest)
    [] -> assertFailure "no strip"
  T.count "┬" top @?= 4
  assertBool (show strip) (all (\l -> "│ " `T.isPrefixOf` l && " │" `T.isSuffixOf` l) (init strip))
  assertBool (show strip) ("└" `T.isPrefixOf` last strip && T.count "┴" (last strip) == 4)
  -- at 80 columns the cards wrap onto two rows of the same box
  b80 <- monthChrome [] 80
  let mids = filter ("├" `T.isPrefixOf`) (T.lines (plainText b80))
  case mids of
    [m] -> assertBool (show m) ("┤" `T.isSuffixOf` m && T.any (`elem` ("┼┴┬" :: String)) m)
    _ -> assertFailure ("one middle rule expected: " ++ show mids)

-- | Thin bars: role glyphs over the track, exactly the width asked for.
unit_thinBar :: Assertion
unit_thinBar = do
  c <- ctxWith ["--color=always"] 140
  n <- ctxWith ["--color=never"] 140
  a <- ctxWith ["--ascii"] 140
  let bar x = thinBar x RInc 10 0.42
  segText (bar c) @?= "━━━━━━━━━━"
  [ (stRole s, t) | Seg s t <- bar c ] @?= [(RInc, "━━━━"), (RRule, "━━━━━━")]
  segText (bar n) @?= "━━━━──────"
  segText (bar a) @?= "====------"
  segText (thinBar c RExp 10 0.001) @?= "━━━━━━━━━━"
  [ t | Seg s t <- thinBar c RExp 10 0.001, stRole s == RExp ] @?= ["━"]
  segText (thinBar c RExp 10 0) @?= "━━━━━━━━━━"
  [ t | Seg s t <- thinBar c RExp 10 0, stRole s == RExp ] @?= []

-- | Paired bars: one row of ▀ (fg income, bg expense, track where a side
-- stops); two thin rows in ASCII or without colour.
unit_pairedBar :: Assertion
unit_pairedBar = do
  c <- ctxWith ["--color=always"] 140
  n <- ctxWith ["--color=never"] 140
  a <- ctxWith ["--ascii"] 140
  let p x = pairedBar x 6 (Just 0.5) (Just (2 / 6))
      cells = [ (stRole s, fmap fst (stBg s)) | Line ss <- bLines (p c), Seg s t <- ss, _ <- T.unpack t ]
  plainText (p c) @?= "▀▀▀▀▀▀"
  cells @?= replicate 2 (RInc, Just RExp) ++ [(RInc, Just RRule)] ++ replicate 3 (RRule, Just RRule)
  plainText (p n) @?= "━━━───\n━━────"
  plainText (p a) @?= "===---\n==----"
  plainText (pairedBar c 4 Nothing Nothing) @?= ""

-- | Mix bars fill their width; in colour every segment is ━ in a tint.
unit_mixBar :: Assertion
unit_mixBar = do
  c <- ctxWith ["--color=always"] 140
  n <- ctxWith ["--color=never"] 140
  let mx = Mix [("NGN", 0.8), ("USD", 0.2)]
  [ (stTint s, t) | Seg s t <- mixBar c KExpense 10 mx ] @?= [(100, "━━━━━━━━"), (60, "━━")]
  segText (mixBar n KExpense 10 mx) @?= "━━━━━━━━══"

-- | A card that asks for a minimum width gets it; the others share the rest.
unit_cardRowMin :: Assertion
unit_cardRowMin = do
  c <- ctxWith [] 140
  let probe cw = str (st RText) (T.replicate cw "x")
      widthsOf b = case T.lines (plainText b) of
        (_ : l : _) -> map T.length (T.splitOn " │ " (T.drop 2 (T.dropEnd 2 l)))
        _ -> []
  widthsOf (cardRowMin c [(0, probe), (26, probe), (0, probe), (0, probe), (0, probe)]) @?= [25, 26, 25, 24, 24]
  widthsOf (cardRowMin c (map ((,) 0) (replicate 5 probe))) @?= widthsOf (cardRow c (replicate 5 probe))
