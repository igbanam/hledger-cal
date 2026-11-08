module Cal.ViewWeekTest where

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
import Cal.Model.Week (weekModel)
import Cal.Types (Cursor (..), defaultCursor)
import Cal.View.Types
import Cal.View.Week (weekDoc)
import Support (cellStyles, cursorCells, envFor)

week44, week53 :: [String]
week44 = ["-W", "-p", "2026-10-28"]
week53 = ["-W", "-p", "2026-12-30"]

weekRender :: [String] -> Int -> Bool -> IO (ViewDoc, Block)
weekRender = weekRenderAt defaultCursor

-- | With the transaction selection at the given cursor.
weekRenderAt :: Cursor -> [String] -> Int -> Bool -> IO (ViewDoc, Block)
weekRenderAt cur args w interactive = do
  env <- envFor args
  let ctx = fromJust (mkRenderCtx env w interactive)
      d = weekDoc ctx (weekModel env cur)
  pure (d, printBlock ctx d)

busy :: [String]
busy = ["-W", "-p", "2026-10-14", "-f", "test/fixtures/week-busy.journal"]

variants :: [(String, [String], Int)]
variants =
  [ ("week-2026-44-" ++ show w, week44, w) | w <- [200, 140, 120, 100, 80] ]
  ++ [ ("week-2026-53-140", week53, 140) ]

utf8 :: Text -> BL.ByteString
utf8 t = BL.fromStrict (TE.encodeUtf8 (t <> "\n"))

test_goldens :: TestTree
test_goldens = testGroup "goldens"
  [ goldenVsString name ("test/golden/" ++ name ++ ".txt") (utf8 . plainText . snd <$> weekRender args w False)
  | (name, args, w) <- variants ]

unit_widths :: Assertion
unit_widths = mapM_ check (variants ++ extra)
  where
    extra = [ ("w44-" ++ show w, week44, w) | w <- [95, 119, 139, 160] ]
      ++ [ ("ascii-" ++ show w, week44 ++ ["--ascii"], w) | w <- [80, 120, 140] ]
      ++ [ ("native-" ++ show w, week44 ++ ["--native"], w) | w <- [80, 120, 140] ]
      ++ [ ("busy-" ++ show w, busy, w) | w <- [80, 120, 200] ]
      ++ [ ("edge-" ++ show w, ["-W", "-p", p, "-f", "test/fixtures/edge.journal"], w)
         | w <- [120, 140], p <- ["2026-10-10", "2026-10-12"] ]
    check (name, args, w) = do
      b <- snd <$> weekRender args w False
      let wide = [ l | l <- T.lines (plainText b), textW l > w ]
      assertBool (name ++ " has lines wider than " ++ show w ++ ": " ++ show wide) (null wide)
      assertBool (name ++ " reports width " ++ show (bWidth b)) (bWidth b <= w)

hasLine :: Text -> [Text] -> Bool
hasLine t frags = any (\l -> all (`T.isInfixOf` l) frags) (T.lines t)

unit_checks :: Assertion
unit_checks = do
  t <- plainText . snd <$> weekRender week44 140 False
  assertBool "title" ("Week 44  26 Oct – 1 Nov 2026" `T.isInfixOf` t)
  assertBool "payroll share" ("96% of it is payroll" `T.isInfixOf` t)
  assertBool "no income" ("none this week" `T.isInfixOf` t)
  assertBool "activity" ("3 of 7 days" `T.isInfixOf` t && "2 currencies" `T.isInfixOf` t)
  assertBool "legend" ("bar = day's spend · grey = native amount @ rate" `T.isInfixOf` t)
  -- the grid (design F5): headers, totals, transactions side by side
  assertBool "day headers" (hasLine t ["│ Mon", "26 │ Tue", "27 │", "│ Sun", "1 Nov │"])
  assertBool "day totals" (hasLine t ["0 │", "−30,500 │", "−1,860,000 │", "−40,000 │"])
  assertBool "descriptions" (hasLine t ["Diesel — generator", "Household payroll — Oct", "Prepaid meter"])
  assertBool "location under the account" (hasLine t ["Port Harcourt", "$700.00 @1,550.00", "−1,085,000", "Abuja"])
  assertBool "empty day" (hasLine t ["│ ·", "│ Diesel"])
  assertBool "fx line" (hasLine t ["FX effect −12,000"])
  assertBool "dashed rule after a txn" (hasLine t ["┄┄┄┄"])
  t53 <- plainText . snd <$> weekRender week53 140 False
  assertBool "week 53 title" ("Week 53" `T.isInfixOf` t53)
  -- below 120 the day list stays (spec §10.5 ruling)
  t100 <- plainText . snd <$> weekRender week44 100 False
  assertBool "tue 27 location" (hasLine t100 ["expenses:fuel", "Port Harcourt", "₦30,500"])
  assertBool "tue 27 row" (hasLine t100 ["Tue 27", "−30,500"])
  assertBool "list empty day" (hasLine t100 ["Mon 26", "0", "·"])
  assertBool "list sun" (hasLine t100 ["Sun 1 Nov"])
  assertBool "list fx line" (hasLine t100 ["    FX effect −12,000 (txn-date rate 1,540.00)"])
  assertBool "list rate" (hasLine t100 ["$700.00", "@1,550", "−1,085,000"])
  assertBool "list has no grid" (not ("┬" `T.isInfixOf` T.unlines (drop 12 (T.lines t100))))
  t80 <- plainText . snd <$> weekRender week44 80 False
  assertBool "activity whole" ("3 of 7 days · 2 currencies" `T.isInfixOf` t80)

-- | The grid is one closed box exactly as wide as the screen at 120 and up.
unit_gridBox :: Assertion
unit_gridBox = mapM_ check [(w, a) | w <- [120, 140, 200], a <- [week44, week44 ++ ["--ascii"], busy]]
  where
    check (w, args) = do
      (d, _) <- weekRender args w False
      let ls = T.lines (plainText (vdBody d))
          edges = if "--ascii" `elem` args then "|+" else "│┌└├┐┘┤" :: String
      assertBool ("lines " ++ show w) (length ls > 12)
      mapM_ (\l -> do
               assertEqual ("width " ++ show w ++ ": " ++ T.unpack l) w (textW l)
               assertBool ("edges " ++ T.unpack l) (T.head l `elem` edges && T.last l `elem` edges)) ls
      -- 7 columns: 8 vertical lines on the header row
      assertEqual "header dividers" 8 (T.count (if "--ascii" `elem` args then "|" else "│") (ls !! 1))

-- | Content columns widen so figure lines stay on one line; at 200 the
-- empty days stay within a cell of each other.
unit_gridColumns :: Assertion
unit_gridColumns = do
  forM_ [140, 200] $ \w -> do
    t <- plainText . vdBody . fst <$> weekRender week44 w False
    assertBool ("figure line whole at " ++ show w) (hasLine t ["$700.00 @1,550.00", "−1,085,000"])
    assertBool ("account whole at " ++ show w) (hasLine t ["expenses:payroll:security", "expenses:utilities:power"])
  t2 <- plainText . vdBody . fst <$> weekRender week44 200 False
  let header = T.lines t2 !! 1
      widths = map T.length (init (drop 1 (T.splitOn "│" header)))
      empties = [ x | (i, x) <- zip [0 :: Int ..] widths, i `elem` [0, 2, 3, 6] ]
  assertBool ("even empty days " ++ show widths) (maximum empties - minimum empties <= 1)
  assertBool ("payroll column widest " ++ show widths) (widths !! 4 == maximum widths)

-- | The 6-row spend bar: the week's largest day fills it, a small day
-- shows a hairline, an empty day nothing; ASCII uses whole cells.
unit_gridBars :: Assertion
unit_gridBars = do
  ls <- T.lines . plainText . vdBody . fst <$> weekRender week44 140 False
  let bar = take 6 (drop 3 ls)
      cols l = map T.strip (init (drop 1 (T.splitOn "│" l)))
  assertBool "friday full" (all (\l -> "███" `T.isPrefixOf` (cols l !! 4)) bar)
  assertBool "tuesday hairline" (map ((!! 1) . cols) bar == replicate 5 "" ++ [cols (last bar) !! 1] && "▁" `T.isPrefixOf` (cols (last bar) !! 1))
  assertBool "monday blank" (all (T.null . (!! 0) . cols) bar)
  la <- T.lines . plainText . vdBody . fst <$> weekRender (week44 ++ ["--ascii"]) 140 False
  let colsA l = map T.strip (init (drop 1 (T.splitOn "|" l)))
  assertBool "ascii hairline" ("___" `T.isPrefixOf` (colsA (la !! 8) !! 1))
  assertBool "ascii full" ("###" `T.isPrefixOf` (colsA (la !! 3) !! 4))

-- | Design S4: the cursor day's whole column is boxed in the cursor role,
-- on the grid's own lines; the day header carries no reverse video; the
-- selected transaction is the one reverse highlight. Print has neither.
unit_cursorBorder :: Assertion
unit_cursorBorder = do
  forM_ [(w, p) | w <- [120, 140, 200], p <- ["2026-10-26", "2026-10-30", "2026-11-01", "2026-10-28"]] $ \(w, p) -> do
    (d, _) <- weekRender ["-W", "-p", p] w True
    let b = vdBody d
        cs = cursorCells b
        lastL = length (bLines b) - 1
        tag = show w ++ " " ++ p
        top = [ c | (0, c, _) <- cs ]
        c0 = minimum top
        c1 = maximum top
        styles = cellStyles b
    assertBool ("cursor cells " ++ tag) (not (null cs))
    assertEqual ("top edge contiguous " ++ tag) [c0 .. c1] top
    assertEqual ("bottom edge " ++ tag) top [ c | (r, c, _) <- cs, r == lastL ]
    -- every middle line: exactly the two side edges, on grid glyphs
    forM_ [1 .. lastL - 1] $ \r ->
      assertEqual ("sides on line " ++ show r ++ " " ++ tag) [c0, c1] [ c | (r', c, _) <- cs, r' == r ]
    assertBool ("border glyphs only " ++ tag) (all (\(_, _, ch) -> ch `elem` ("─│┌┐└┘├┤┬┴┼" :: String)) cs)
    assertBool ("monday box starts at the edge " ++ tag) (p /= "2026-10-26" || c0 == 0)
    assertBool ("sunday box ends at the edge " ++ tag) (p /= "2026-11-01" || c1 == w - 1)
    assertBool ("no reverse video on the header " ++ tag) (not (any (stInverse . snd) (styles !! 1)))
    let inv = [ r | (r, cells) <- zip [0 :: Int ..] styles, any (stInverse . snd) cells ]
    if p == "2026-10-30"
      then do
        -- the description's lines, from the cursor row (it wraps at 120)
        r <- maybe (assertFailure "no cursor row") pure (vdCursorRow d)
        assertEqual ("the selected description is reversed " ++ tag) [r .. r + length inv - 1] inv
        assertBool ("one or two description lines " ++ tag) (length inv `elem` [1, 2])
      else assertEqual ("nothing reversed on an empty day " ++ tag) [] inv
  (d', _) <- weekRender ["-W", "-p", "2026-10-30"] 140 False
  assertEqual "print: no cursor border" [] (cursorCells (vdBody d'))
  assertBool "print: no reverse video" (not (any (any (stInverse . snd)) (cellStyles (vdBody d'))))

-- | ↑↓ selects within a long day: the reversed description, the cursor
-- row and the selected transaction's last row follow the selection.
unit_selection :: Assertion
unit_selection = do
  forM_ [0, 5, 11] $ \i -> do
    (d, _) <- weekRenderAt (Cursor i 0) busy 140 True
    let ls = T.lines (plainText (vdBody d))
        name = "Errand " <> T.pack (show (i + 1))
        styles = cellStyles (vdBody d)
        inv = [ r | (r, cells) <- zip [0 :: Int ..] styles, any (stInverse . snd) cells ]
    r <- maybe (assertFailure "no cursor row") pure (vdCursorRow d)
    assertBool ("cursor row holds " ++ T.unpack name) ((name <> " ") `T.isInfixOf` (ls !! r))
    assertEqual "only the selected description is reversed" [r] inv
    end <- maybe (assertFailure "no selection end") pure (vdCursorLast d)
    assertBool "the selection ends on its dashed rule" ("┄┄┄" `T.isInfixOf` (ls !! end) && end > r)
    assertBool "its posting lies inside" (any ("−" `T.isInfixOf`) (take (end - r) (drop r ls)))

unit_ascii :: Assertion
unit_ascii = do
  t <- plainText . snd <$> weekRender (week44 ++ ["--ascii"]) 140 False
  let bad = [ c | c <- T.unpack t, fromEnum c > 127, c `notElem` ("₦$€₹—" :: String) ]
  assertBool ("non-ASCII glyphs: " ++ show (take 20 bad)) (null bad)

unit_cursor :: Assertion
unit_cursor = do
  (d, _) <- weekRender ["-W", "-p", "2026-10-30"] 140 True
  case vdCursorRow d of
    Nothing -> assertFailure "cursor row not set when interactive"
    Just r -> assertBool "cursor on payroll txn" ("Household payroll" `T.isInfixOf` (T.lines (plainText (vdBody d)) !! r))
  (d', _) <- weekRender ["-W", "-p", "2026-10-30"] 140 False
  vdCursorRow d' @?= Nothing
  (d2, _) <- weekRender ["-W", "-p", "2026-10-28"] 140 True
  case vdCursorRow d2 of
    Nothing -> assertFailure "no cursor row on empty day"
    Just r -> assertBool "day header" (hasLine (T.lines (plainText (vdBody d2)) !! r) ["Wed", "28"])
  vdCursorLast d2 @?= Nothing
  -- the list (below 120) keeps its cursor rows
  (d3, _) <- weekRender ["-W", "-p", "2026-10-28"] 100 True
  case vdCursorRow d3 of
    Nothing -> assertFailure "no cursor row on empty day (list)"
    Just r -> assertBool "day label" ("Wed 28" `T.isInfixOf` (T.lines (plainText (vdBody d3)) !! r))

unit_asciiKeys :: Assertion
unit_asciiKeys = do
  (d, _) <- weekRender (week44 ++ ["--ascii"]) 140 False
  assertBool "transaction hint kept" ("transaction" `elem` map snd (vdKeys d))
