module Cal.ViewMonthTest where

import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit

import Cal.Doc
import Cal.Model.Month (monthModel)
import Cal.Theme (Role (..))
import Cal.Types (Settings (..))
import Cal.View.Month (monthDoc)
import Cal.View.Types
import Support (cellStyles, cursorCells, edgePath, envFor)

monthRender :: [String] -> Int -> IO Block
monthRender args w = do
  env <- envFor args
  let ctx = fromJust (mkRenderCtx env w False)
  pure (printBlock ctx (monthDoc ctx (monthModel env)))

oct :: [String]
oct = ["-M", "-p", "2026-10"]

variants :: [(String, [String], Int)]
variants =
  [ ("month-2026-10-" ++ show w, oct, w) | w <- [140, 120, 100, 80] ]
  ++ [ ("month-2026-10-usd-140", oct ++ ["-X", "$"], 140)
     , ("month-2026-10-native-140", oct ++ ["--native"], 140)
     , ("month-2026-10-ascii-140", oct ++ ["--ascii"], 140)
     , ("month-2026-10-30-140", ["-M", "-p", "2026-10-30"], 140)
     , ("month-2026-08-140", ["-M", "-p", "2026-08"], 140)
     , ("month-2026-12-140", ["-M", "-p", "2026-12"], 140)
     , ("month-2026-10-edge-80", oct ++ ["-f", edgePath], 80) ]

utf8 :: Text -> BL.ByteString
utf8 t = BL.fromStrict (TE.encodeUtf8 (t <> "\n"))

test_goldens :: TestTree
test_goldens = testGroup "goldens"
  [ goldenVsString name ("test/golden/" ++ name ++ ".txt") (utf8 . plainText <$> monthRender args w)
  | (name, args, w) <- variants ]

unit_widths :: Assertion
unit_widths = mapM_ check (variants ++ extra)
  where
    extra = [ ("edge-" ++ show w, oct ++ ["-f", edgePath], w) | w <- [80, 95, 100, 119, 120, 139, 140, 200] ]
      ++ [ ("oct-" ++ show w, oct, w) | w <- [95, 119, 139, 200] ]
      ++ [ ("edge-10-" ++ show w, edge10, w) | w <- [80, 100, 120, 140] ]
    check (name, args, w) = do
      b <- monthRender args w
      let wide = [ l | l <- T.lines (plainText b), textW l > w ]
      assertBool (name ++ " has lines wider than " ++ show w ++ ": " ++ show wide) (null wide)
      assertBool (name ++ " reports width " ++ show (bWidth b)) (bWidth b <= w)

unit_cursor :: Assertion
unit_cursor = do
  env <- envFor ["-M", "-p", "2026-10-30"]
  let ctx = fromJust (mkRenderCtx env 140 True)
      d = monthDoc ctx (monthModel env)
  case vdCursorRow d of
    Nothing -> assertFailure "cursor row not set when interactive"
    Just r -> do
      let l = T.lines (plainText (vdBody d)) !! r
      assertBool ("cursor row holds 30: " ++ show l) (" 30 " `T.isInfixOf` l)
  let ctx' = fromJust (mkRenderCtx env 140 False)
  vdCursorRow (monthDoc ctx' (monthModel env)) @?= Nothing

-- | Lines of the rendered text containing all the given fragments.
hasLine :: Text -> [Text] -> Bool
hasLine t frags = any (\l -> all (`T.isInfixOf` l) frags) (T.lines t)

unit_checks :: Assertion
unit_checks = do
  t <- plainText <$> monthRender oct 140
  let ls = T.lines t
      firstWeek = [ l | l <- ls, "+2,455,000" `T.isInfixOf` l ]
  assertBool "first week net" (not (null firstWeek))
  -- the week-net column of the first week: net, then in, then out on the following lines
  let after = dropWhile (not . ("+2,455,000" `T.isInfixOf`)) ls
  assertBool "in 2,500,000" (any ("in 2,500,000" `T.isInfixOf`) (take 2 after))
  assertBool "out 45,000" (any ("out 45,000" `T.isInfixOf`) (take 3 after))
  assertBool "30 USD" (hasLine t ["30", "USD"])
  assertBool "30 Oct net" (hasLine t ["−1,860,000"])
  assertBool "30 Oct native" (hasLine t ["$1,200.00"])
  t30 <- plainText <$> monthRender ["-M", "-p", "2026-10-30"] 140
  assertBool "30 Oct note" ("at the txn-date rate (₦1,540.00) this was −1,848,000 · FX effect −12,000" `T.isInfixOf` t30)
  assertBool "30 Oct title" (hasLine t30 ["Fri 30 Oct 2026", "1 txn · 2 postings · USD"])
  assertBool "30 Oct rate" (hasLine t30 ["expenses:payroll:security", "$700.00", "@ ₦1,550.00", "−1,085,000"])
  assertBool "rates title" ("Rates · 31 Oct" `T.isInfixOf` t)
  assertBool "rates heading" ("₦ per 1 unit · Δ month" `T.isInfixOf` t)
  assertBool "stale AED" (hasLine t ["AED •", "stale · 14 Aug"])
  -- print: the legend without the cursor note, and no key hints (spec §8)
  assertBool "legend" ("grey = native amount" `T.isInfixOf` t)
  assertBool "no cursor note" (not ("day under cursor" `T.isInfixOf` t))
  assertBool "no key hints" (not ("filter account" `T.isInfixOf` t))
  env <- envFor oct
  let ictx = fromJust (mkRenderCtx env 140 True)
      idoc = monthDoc ictx (monthModel env)
  assertBool "interactive legend" ("▢ day under cursor · grey = native amount" `T.isInfixOf` plainText (vdLegend idoc))
  assertBool "interactive bottom has no footer" (not ("filter account" `T.isInfixOf` plainText (vdBottom idoc)))

unit_usd :: Assertion
unit_usd = do
  t <- plainText <$> monthRender ["-M", "-p", "2026-10-30", "-X", "$"] 140
  assertBool "native rows" (hasLine t ["expenses:payroll:security", "$700.00", "native", "−700.00"])
  assertBool "no conversion" ("already in USD · no conversion · no FX effect" `T.isInfixOf` t)
  assertBool "-X makes USD the default" ("$ per 1 unit · Δ month" `T.isInfixOf` t)
  assertBool "via NGN" (hasLine t ["EUR", "via NGN · 20 Oct"])

-- | After switching away from the default base (the TUI's `c`), the rates
-- heading reads "per $1".
unit_switchedHeading :: Assertion
unit_switchedHeading = do
  env <- envFor (oct ++ ["-X", "$"])
  let ctx0 = fromJust (mkRenderCtx env 140 False)
      ctx = ctx0 { rcSettings = (rcSettings ctx0) { sDefaultBase = "₦" } }
      t = plainText (printBlock ctx (monthDoc ctx (monthModel env)))
  assertBool "per $1" ("per $1 · Δ month" `T.isInfixOf` t)

unit_tier80WeekRow :: Assertion
unit_tier80WeekRow = do
  t <- plainText <$> monthRender oct 80
  assertBool "week row" ("week net +2,455,000 · in 2,500,000 · out 45,000" `T.isInfixOf` t)

edge10 :: [String]
edge10 = ["-M", "-p", "2026-10-10", "-f", edgePath]

-- | Review Focus 5: the long description and account are truncated, the
-- huge amounts are not.
unit_edgeLong :: Assertion
unit_edgeLong = do
  t <- plainText <$> monthRender edge10 80
  assertBool "description truncated" ("An extremel…" `T.isInfixOf` t)
  assertBool "account elided" ("mergency-callout" `T.isInfixOf` t && not ("expenses:household:maintenance" `T.isInfixOf` t))
  assertBool "amounts whole" (hasLine t ["₦123,456,789,012", "−123,456,789,012"])

unit_ascii :: Assertion
unit_ascii = do
  t <- plainText <$> monthRender (oct ++ ["--ascii"]) 140
  let bad = [ c | c <- T.unpack t, fromEnum c > 127, c `notElem` ("₦$€₹—" :: String) ]
  assertBool ("non-ASCII glyphs: " ++ show (take 20 bad)) (null bad)

-- | The cursor day's whole cell is drawn by its border in the cursor role
-- (design S4); no label is highlighted; print has no cursor.
unit_cursorBorder :: Assertion
unit_cursorBorder = do
  env <- envFor ["-M", "-p", "2026-10-30"]
  let ctx = fromJust (mkRenderCtx env 140 True)
      d = monthDoc ctx (monthModel env)
      body = vdBody d
      cs = cursorCells body
      rows = [ r | (r, _, _) <- cs ]
      colsAt r = [ c | (r', c, _) <- cs, r' == r ]
      bodyLines = T.lines (plainText body)
  r <- maybe (assertFailure "no cursor row") pure (vdCursorRow d)
  assertBool ("cursor cells are border glyphs: " ++ show cs) (all (\(_, _, ch) -> ch `elem` ("─│┼┴┬├┤" :: String)) cs)
  (minimum rows, maximum rows) @?= (r - 1, r + 3)
  -- the top rule spans the cell, from one divider to the next
  let top = colsAt (r - 1)
      (c0, c1) = (minimum top, maximum top)
  assertBool ("30 sits inside the box: " ++ show (c0, c1)) (" 30 " `T.isInfixOf` T.take (c1 - c0) (T.drop (c0 + 1) (bodyLines !! r)))
  colsAt (r + 3) @?= top
  mapM_ (\i -> colsAt i @?= [c0, c1]) [r .. r + 2]
  -- no reverse-video label anywhere in the body
  assertBool "no inverse cell" (not (any (stInverse . snd) (concat (cellStyles body))))
  let ctx' = fromJust (mkRenderCtx env 140 False)
  cursorCells (vdBody (monthDoc ctx' (monthModel env))) @?= []

interactiveDoc :: [String] -> Int -> IO ViewDoc
interactiveDoc args w = do
  env <- envFor args
  let ctx = fromJust (mkRenderCtx env w True)
  pure (monthDoc ctx (monthModel env))

-- | The calendar is one bordered box at every tier (design F4): corners on
-- the first and last lines, a left and a right edge on every other line,
-- and the box as wide as the page.
unit_outerBorder :: Assertion
unit_outerBorder = mapM_ check [200, 140, 120, 100, 80]
  where
    check w = do
      d <- interactiveDoc oct w
      ls <- case T.lines (plainText (vdBody d)) of
        xs@(_ : _ : _) -> pure xs
        xs -> assertFailure ("too few body lines: " ++ show xs)
      let firstLast l = (T.head l, T.last l)
          mids = drop 1 (init ls)
      fmap firstLast (take 1 ls) @?= [('┌', '┐')]
      firstLast (last ls) @?= ('└', '┘')
      assertBool (show w ++ ": every line has a left and a right edge")
        (all (\l -> T.head l `elem` ("│├" :: String) && T.last l `elem` ("│┤" :: String)) mids)
      assertBool (show w ++ ": the box fills the width") (all ((== w) . textW) ls)

-- | The week-net column sits on the panel tint (design F4), its padding
-- included; the day cells don't.
unit_weekNetTint :: Assertion
unit_weekNetTint = do
  d <- interactiveDoc oct 140
  let rows = cellStyles (vdBody d)
      tinted' (_, s) = stBg s == Just (RPanel, 100)
      ls = T.lines (plainText (vdBody d))
      hdr = rows !! 1
      wn = [ (c, s) | (c, s) <- hdr, tinted' (c, s) ]
  assertBool "header week net is tinted" ("week net" `T.isInfixOf` T.pack (map fst wn))
  -- the first week's net line: every cell between the last divider and the edge is tinted
  let i = length (takeWhile (not . ("+2,455,000" `T.isInfixOf`)) ls)
      row = rows !! i
      lastDiv = last [ k | (k, (ch, s)) <- zip [0 :: Int ..] row, ch == '│', stRole s == RRule, k < length row - 1 ]
      col = take (length row - lastDiv - 2) (drop (lastDiv + 1) row)
  assertBool ("week-net cells tinted: " ++ show (map fst col)) (all tinted' col && not (null col))
  assertBool "day cells untinted" (not (any tinted' (take lastDiv row)))

-- | The cursor box is closed on all four sides at every tier, for a
-- Monday, a Friday and a Sunday (design S4; F1 review: Tier80 and Monday).
unit_cursorBoxClosed :: Assertion
unit_cursorBoxClosed = sequence_
  [ check w day | w <- [200, 140, 120, 100, 80], day <- ["2026-10-05", "2026-10-30", "2026-10-11"] ]
  where
    check w day = do
      d <- interactiveDoc ["-M", "-p", day] w
      let cs = cursorCells (vdBody d)
          tag = day ++ " at " ++ show w
          rows = [ r | (r, _, _) <- cs ]
          (r0, r1) = (minimum rows, maximum rows)
          colsAt r = [ c | (r', c, _) <- cs, r' == r ]
          top = colsAt r0
          (c0, c1) = (minimum top, maximum top)
      assertBool (tag ++ ": no cursor cells") (not (null cs))
      assertEqual (tag ++ ": top edge is contiguous") [c0 .. c1] top
      assertEqual (tag ++ ": bottom edge matches the top") top (colsAt r1)
      mapM_ (\r -> assertEqual (tag ++ ": sides on line " ++ show r) [c0, c1] (colsAt r)) [r0 + 1 .. r1 - 1]
      assertBool (tag ++ ": a box, not a line") (r1 - r0 >= 2 && c1 - c0 >= 2)
      Just r <- pure (vdCursorRow d)
      assertEqual (tag ++ ": the cursor row is the box's first inner line") (r0 + 1) r

-- | The day detail and rates panels open with a title row inside the box
-- (design F4): the day bold on the left and its counts on the right; the
-- rates date on the left and the heading on the right.
unit_panelTitles :: Assertion
unit_panelTitles = do
  t30 <- plainText <$> monthRender ["-M", "-p", "2026-10-30"] 140
  let ls = T.lines t30
      titleL = [ l | l <- ls, "Fri 30 Oct 2026" `T.isInfixOf` l ]
  assertBool ("detail title row: " ++ show titleL) (hasLine t30 ["│ Fri 30 Oct 2026 ", " 1 txn · 2 postings · USD │"])
  assertBool "rates title row" (hasLine t30 ["│ Rates · 31 Oct ", " ₦ per 1 unit · Δ month │"])
  -- no title in the top border any more
  assertBool "plain top borders" (not (any ("┌─ " `T.isPrefixOf`) ls))
  -- the FX note follows the postings directly
  let after = dropWhile (not . ("expenses:payroll:domestic" `T.isInfixOf`)) ls
  assertBool "note under the postings" ("at the txn-date rate" `T.isInfixOf` (after !! 1))

-- | Stale rates sort after the fresh ones, as in the artboard.
unit_ratesOrder :: Assertion
unit_ratesOrder = do
  t <- plainText <$> monthRender oct 140
  let codes = [ c | l <- T.lines t, c <- ["USD", "AED", "EUR", "INR", "KWD"], ("│ " <> c <> " ") `T.isInfixOf` l ]
  codes @?= ["USD", "EUR", "INR", "KWD", "AED"]

-- | At full width the week-net column is as wide as a day column, give or
-- take one cell (design F4: eight even columns).
unit_evenColumns :: Assertion
unit_evenColumns = mapM_ check [200, 140]
  where
    check w = do
      d <- interactiveDoc oct w
      let topL = T.concat (take 1 (T.lines (plainText (vdBody d))))
          joints = [ k | (k, ch) <- zip [0 :: Int ..] (T.unpack topL), ch `elem` ("┌┬┐" :: String) ]
          widths = zipWith (-) (drop 1 joints) joints
      assertBool (show w ++ ": column widths " ++ show widths) (maximum widths - minimum widths <= 1 && length widths == 8)
