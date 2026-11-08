module Cal.ViewQuarterTest where

import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit

import Cal.Doc
import Cal.Theme (Role (..))
import Cal.Format (fmtSigned)
import Cal.Model.Common (Kpis (..))
import Cal.Model.Quarter (QuarterCard (..), QuarterModel (..), quarterModel)
import Cal.View.Quarter (quarterDoc)
import Cal.View.Types
import Support (cellStyles, cursorCells, envFor)

quarterRender :: [String] -> Int -> IO Block
quarterRender extra w = do
  env <- envFor extra
  let ctx = fromJust (mkRenderCtx env w False)
  pure (printBlock ctx (quarterDoc ctx (quarterModel env)))

variants :: [(String, [String], Int)]
variants = [ ("quarter-2026-" ++ show w, ["-Q"], w) | w <- [140, 120, 100, 80] ]
  ++ [ ("quarter-2026-native-" ++ show w, ["-Q", "--native"], w) | w <- [140, 100] ]

utf8 :: Text -> BL.ByteString
utf8 t = BL.fromStrict (TE.encodeUtf8 (t <> "\n"))

test_goldens :: TestTree
test_goldens = testGroup "goldens"
  [ goldenVsString name ("test/golden/" ++ name ++ ".txt") (utf8 . plainText <$> quarterRender args w)
  | (name, args, w) <- variants ]

unit_widths :: Assertion
unit_widths = mapM_ check (variants ++ [("quarter-ascii-100", ["-Q", "--ascii"], 100), ("quarter-native-120", ["-Q", "--native"], 120)])
  where
    check (name, args, w) = do
      b <- quarterRender args w
      let wide = [ l | l <- T.lines (plainText b), textW l > w ]
      assertBool (name ++ " has lines wider than " ++ show w ++ ": " ++ show wide) (null wide)
      assertBool (name ++ " reports width " ++ show (bWidth b)) (bWidth b <= w)

unit_cursor :: Assertion
unit_cursor = do
  env <- envFor ["-Q"]
  let ctx = fromJust (mkRenderCtx env 140 True)
      d = quarterDoc ctx (quarterModel env)
      body = T.lines (plainText (vdBody d))
  case vdCursorRow d of
    Nothing -> assertFailure "cursor row not set when interactive"
    Just r -> assertBool "cursor row holds the Q4 title" ("Q4" `T.isInfixOf` (body !! r))
  let ctx' = fromJust (mkRenderCtx env 140 False)
  vdCursorRow (quarterDoc ctx' (quarterModel env)) @?= Nothing

unit_checks :: Assertion
unit_checks = do
  t <- plainText <$> quarterRender ["-Q"] 140
  assertBool "Q4 expenses" (("\8722" <> "2,715,000") `T.isInfixOf` t)
  assertBool "stale strip" ("\8226 AED rate is 84 days old" `T.isInfixOf` t)
  assertBool "in progress" ("in progress \183 to 6 Nov" `T.isInfixOf` t)
  assertBool "closed" ("closed" `T.isInfixOf` t)

unit_ascii :: Assertion
unit_ascii = do
  t <- plainText <$> quarterRender ["-Q", "--ascii"] 100
  assertBool "no en dash" (not ("\8211" `T.isInfixOf` t))

-- | The cursor quarter's card is drawn by its border in the cursor role
-- (design S4); its Q label is not highlighted; print has no cursor.
unit_cursorBorder :: Assertion
unit_cursorBorder = do
  env <- envFor ["-Q"]
  let ctx = fromJust (mkRenderCtx env 140 True)
      body = vdBody (quarterDoc ctx (quarterModel env))
      cs = cursorCells body
      ls = T.lines (plainText body)
      rows = [ r | (r, _, _) <- cs ]
      cols = [ c | (_, c, _) <- cs ]
  assertBool "cursor border drawn" (not (null cs))
  assertBool ("border glyphs only: " ++ show cs) (all (\(_, _, ch) -> ch `elem` ("─│┌┐└┘" :: String)) cs)
  -- the box around Q4: the right-hand card of the second row
  let r0 = minimum rows
      r1 = maximum rows
  assertBool "Q4 inside" ("Q4" `T.isInfixOf` (ls !! (r0 + 1)))
  assertBool "right-hand card" (minimum cols > 70 && maximum cols == 139)
  length cs @?= 2 * (maximum cols - minimum cols + 1) + 2 * (r1 - r0 - 1)
  assertBool "no inverse Q label" (not (any (\(ch, s) -> ch == 'Q' && stInverse s) (concat (cellStyles body))))
  let ctx' = fromJust (mkRenderCtx env 140 False)
  cursorCells (vdBody (quarterDoc ctx' (quarterModel env))) @?= []

-- | The inner text of the left-hand card of the first grid row (Q1), line
-- by line, without its borders.
q1Inner :: Block -> [Text]
q1Inner b = [ T.strip (T.take (cw - 2) (T.drop 1 l)) | l <- takeWhile (not . corner) (drop 1 card) ]
  where
    corner l = any (`T.isPrefixOf` l) ["└", "┌", "+"]
    card = dropWhile (not . corner) (T.lines (plainText b))
    cw = case card of
      (top : _) -> T.length (T.takeWhile (/= ' ') top)
      [] -> 0

-- | At ≥ 140 a card follows the design's rhythm (F3): header, blank,
-- labels, figures, blank, the in bar with its legend beside it, blank, the
-- out bar, blank, then one row per month (paired ▀ bar, net at the edge).
unit_cardRhythm :: Assertion
unit_cardRhythm = do
  env <- envFor ["-Q", "--color=always"]
  let ctx = fromJust (mkRenderCtx env 140 False)
      body = vdBody (quarterDoc ctx (quarterModel env))
      ls = q1Inner body
      starts p i = assertBool (show i ++ ": " ++ show (ls !! i)) (p `T.isPrefixOf` (ls !! i))
  length ls @?= 12
  starts "Q1" 0
  mapM_ (\i -> ls !! i @?= "") [1, 4, 6, 8]
  starts "income" 2
  starts "+7,500,000" 3
  starts "in " 5
  assertBool "in legend beside its bar" ("━  ₦ 100.0%" `T.isInfixOf` (ls !! 5))
  starts "out " 7
  assertBool "out legend beside its bar" ("━  $ 85.6% · ₦ 14.4%" `T.isInfixOf` (ls !! 7))
  -- the mix bar is a thin bar about 40% of the card wide, not the whole card
  let barW = T.length (T.filter (`elem` ("━═┅┄" :: String)) (ls !! 7))
  assertBool ("mix bar width " ++ show barW) (barW >= 15 && barW <= 30)
  forM_ (zip [9, 10, 11] ["Jan", "Feb", "Mar"]) $ \(i, mon) -> do
    starts mon i
    assertBool (T.unpack mon ++ " paired bar") ("▀" `T.isInfixOf` (ls !! i))
  assertBool "Jan net at the edge" ("+424,000" `T.isSuffixOf` (ls !! 9))

-- | Month rows: the net (▲ overspent, … open) at the card's right edge, no
-- in/out figures; a future month reads "· no postings yet (<hint>)".
unit_monthRows :: Assertion
unit_monthRows = do
  forM_ [200, 140] $ \w -> do
    env <- envFor ["-Q", "--color=always"]
    let ctx = fromJust (mkRenderCtx env w False)
        ls = T.lines (plainText (vdBody (quarterDoc ctx (quarterModel env))))
        has p = assertBool (show w ++ ": " ++ T.unpack p) (any (p `T.isInfixOf`) ls)
    has "−329,000 ▲ │"
    has "+2,090,000 … │"
    has "│ Dec  · no postings yet (forecast "
    has "$1,200.00 ≈ −1,860,000) "
    assertBool "no in/out figures" (not (any ("2,076,000" `T.isInfixOf`) ls))

-- | Without colour or in ASCII a paired bar is two rows (in, then out).
unit_monthRowsPlain :: Assertion
unit_monthRowsPlain = forM_ [["-Q", "--color=never"], ["-Q", "--ascii"]] $ \args -> do
  env <- envFor args
  let ctx = fromJust (mkRenderCtx env 140 False)
      ls = q1Inner (vdBody (quarterDoc ctx (quarterModel env)))
  length ls @?= 15
  assertBool (show args) ("Jan " `T.isPrefixOf` (ls !! 9) && "+424,000" `T.isSuffixOf` (ls !! 9))
  assertBool (show args) (not (T.any (`elem` ("JFM+" :: String)) (ls !! 10)) && not (T.null (ls !! 10)))

-- | Purple is the cursor only: with the cursor on Q1, the current quarter
-- (Q4) is marked by its chip alone, with a plain border.
unit_currentQuarterChipOnly :: Assertion
unit_currentQuarterChipOnly = do
  env <- envFor ["-Q", "-p", "2026-02"]
  let ctx = fromJust (mkRenderCtx env 140 True)
      body = vdBody (quarterDoc ctx (quarterModel env))
      cs = cursorCells body
      accent = [ (r, c) | (r, l) <- zip [0 :: Int ..] (cellStyles body), (c, (_, s)) <- zip [0 :: Int ..] l, stRole s == RCursor ]
  assertBool "cursor border drawn" (not (null cs))
  assertBool ("cursor on Q1 only: " ++ show cs) (all (\(_, c, _) -> c < 70) cs)
  assertBool ("no accent cells: " ++ show accent) (null accent)
  assertBool "Q4 chip" ("in progress · to 6 Nov" `T.isInfixOf` plainText body)

-- | Native KPI figures are never cut (rule I7): every amount of every
-- card's income, expenses, net and FX figure appears whole, one amount per
-- line where the figure has to wrap.
unit_nativeFiguresWhole :: Assertion
unit_nativeFiguresWhole = forM_ [200, 140, 120, 100, 80] $ \w -> do
  env <- envFor ["-Q", "--native"]
  let ctx = fromJust (mkRenderCtx env w False)
      f = rcFmt ctx
      m = quarterModel env
      t = plainText (vdBody (quarterDoc ctx m))
      minus = "\8722"
      amounts x = filter (not . T.null) (map T.strip (T.splitOn "\0" (T.replace " + " "\0+ " (T.replace (" " <> minus <> " ") ("\0" <> minus <> " ") x))))
      figs = concat [ [fmtSigned f (kIncome k), fmtSigned f (negate (kExpenses k)), fmtSigned f (kNet k)]
                      | c <- qmCards m, let k = qcKpis c ]
  forM_ (concatMap amounts figs) $ \a ->
    assertBool (show w ++ ": " ++ T.unpack a) (a `T.isInfixOf` t)
  assertBool (show w ++ ": no cut figure") (not (any ("\8230 " `T.isPrefixOf`) (T.words t)))
  assertBool (show w ++ ": no bare ellipsis") (not ("  \8230 " `T.isInfixOf` t))
