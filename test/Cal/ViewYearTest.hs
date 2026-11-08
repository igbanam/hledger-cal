module Cal.ViewYearTest where

import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAscii)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit

import Cal.Doc
import Cal.Theme (Role (..))
import Cal.Model.Year (yearModel)
import Cal.Types (defaultCursor)
import Cal.View (faceDoc)
import Cal.View.Types
import Cal.View.Year (yearDoc)
import Support (cellStyles, cursorCells, envFor)

yearRender :: [String] -> Int -> IO Block
yearRender extra w = do
  env <- envFor extra
  let ctx = fromJust (mkRenderCtx env w False)
  pure (printBlock ctx (yearDoc ctx (yearModel env)))

variants :: [(String, [String], Int)]
variants =
  [ ("year-2026-" ++ show w, ["-Y"], w) | w <- [140, 120, 100, 80] ]
  ++ [ ("year-2026-native-140", ["-Y", "--native"], 140)
     , ("year-2025-140", ["-Y", "-p", "2025"], 140) ]

utf8 :: Text -> BL.ByteString
utf8 t = BL.fromStrict (TE.encodeUtf8 (t <> "\n"))

test_goldens :: TestTree
test_goldens = testGroup "goldens"
  [ goldenVsString name ("test/golden/" ++ name ++ ".txt") (utf8 . plainText <$> yearRender args w)
  | (name, args, w) <- variants ]

unit_widths :: Assertion
unit_widths = mapM_ check variants
  where
    check (name, args, w) = do
      b <- yearRender args w
      let wide = [ l | l <- T.lines (plainText b), textW l > w ]
      assertBool (name ++ " has lines wider than " ++ show w ++ ": " ++ show wide) (null wide)
      assertBool (name ++ " reports width " ++ show (bWidth b)) (bWidth b <= w)

unit_cursor :: Assertion
unit_cursor = do
  env <- envFor ["-Y"]
  let ctx = fromJust (mkRenderCtx env 140 True)
      d = yearDoc ctx (yearModel env)
  assertBool "cursor row set when interactive" (vdCursorRow d /= Nothing)
  let ctx' = fromJust (mkRenderCtx env 140 False)
  vdCursorRow (yearDoc ctx' (yearModel env)) @?= Nothing

unit_checks :: Assertion
unit_checks = do
  t <- plainText <$> yearRender ["-Y"] 140
  assertBool "Aug overspent" (any (\l -> "Aug" `T.isInfixOf` l && "▲" `T.isInfixOf` l) (T.lines t))
  -- in the Dec card at 140 the hint takes two lines; its figures stay whole
  assertBool "Dec forecast" ("forecast Household payroll" `T.isInfixOf` t && "$1,200.00 ≈ −1,860,000" `T.isInfixOf` t)

-- | Every view's key hints are ASCII in ASCII mode.
unit_asciiKeys :: Assertion
unit_asciiKeys = forM_ [["-Y"], ["-Q"], ["-M"], ["-W"], ["--commodities"]] $ \face -> do
  env <- envFor (face ++ ["--ascii"])
  let ctx = fromJust (mkRenderCtx env 140 True)
      d = faceDoc ctx env defaultCursor
  assertBool (show face ++ " keys not ASCII: " ++ show (vdKeys d))
    (all (\(k, l) -> T.all isAscii k && T.all isAscii l) (vdKeys d))

-- | The ≥ 140 grid (design F2): a Q box and three month cards per quarter.
yearBody :: [String] -> Int -> Bool -> IO Block
yearBody extra w interactive = do
  env <- envFor extra
  let ctx = fromJust (mkRenderCtx env w interactive)
  pure (vdBody (yearDoc ctx (yearModel env)))

-- | Cells of the given role whose character is one of @cs@.
roleCells :: Role -> String -> Block -> [(Int, Int, Char)]
roleCells role cs b =
  [ (i, col, c) | (i, row) <- zip [0 ..] (cellStyles b), (col, (c, s)) <- zip [0 ..] row
  , stRole s == role, c `elem` cs ]

-- | The rectangle of a month card: (top, left) to (bottom, right) of the
-- border around the line holding @name@ at its first column.
cardRect :: Text -> Block -> ((Int, Int), (Int, Int))
cardRect name b = ((r0, c0), (r1, c1))
  where
    ls = T.lines (plainText b)
    (ri, l) = firstOf ("no " ++ T.unpack name ++ " card") [ (i, x) | (i, x) <- zip [0 ..] ls, (" " <> name <> " ") `T.isInfixOf` x ]
    nameCol = T.length (fst (T.breakOn (" " <> name <> " ") l)) + 1
    -- the left border sits two cells before the name (border, padding)
    c0 = nameCol - 2
    row = ls !! ri
    c1 = c0 + T.length (T.takeWhile (\ch -> ch /= '│' && ch /= '┆') (T.drop (c0 + 1) row)) + 1
    r0 = ri - 1
    firstOf msg xs = case xs of
      (x : _) -> x
      [] -> error msg
    r1 = firstOf "no bottom border" [ i | (i, x) <- drop (ri + 1) (zip [0 ..] ls), T.index x c0 == '└' ]

unit_gridLayout :: Assertion
unit_gridLayout = do
  b <- yearBody ["-Y"] 140 False
  let ls = T.lines (plainText b)
  -- four quarter rows, each a Q box then three cards
  length (filter ("│ Q" `T.isPrefixOf`) ls) @?= 4
  forM_ (T.words "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec") $ \mon ->
    assertBool (T.unpack mon ++ " card") (any (\l -> ("│ " <> mon) `T.isInfixOf` l || ("┆ " <> mon) `T.isInfixOf` l) ls)
  assertBool "in and out on their own rows" (any (\l -> "2,500,000" `T.isInfixOf` l && "━" `T.isInfixOf` l) ls)
  assertBool "Q1 % spent" (any ("83.5% spent" `T.isInfixOf`) ls)
  assertBool "Q4 open" (any ("open · to 6 Nov" `T.isInfixOf`) ls)
  assertBool "Nov to 6th chip, net with …" (any (\l -> "Nov  to 6th " `T.isInfixOf` l && "+2,090,000 …" `T.isInfixOf` l) ls)
  assertBool "Aug ▲" (any (\l -> "Aug" `T.isInfixOf` l && "−329,000 ▲" `T.isInfixOf` l) ls)
  assertBool "Dec no postings" (any ("no postings yet" `T.isInfixOf`) ls)
  -- the chip is reverse video
  assertBool "chip inverse" (any (\(ch, s) -> ch == 't' && stInverse s) (concat (cellStyles b)))
  -- Aug's border is the expense tint, Dec's dashed
  let ((a0, ac0), (a1, ac1)) = cardRect "Aug" b
      augBorder = [ x | x@(r, c, _) <- roleCells RExp "─│┌┐└┘" b, r >= a0, r <= a1, c >= ac0, c <= ac1 ]
  length augBorder @?= 2 * (ac1 - ac0 + 1) + 2 * (a1 - a0 - 1)
  -- purple means the cursor only: the open month (Nov) has a plain border
  roleCells RCursor "─│┌┐└┘" b @?= []
  let ((n0, nc0), (n1, nc1)) = cardRect "Nov" b
      onEdge r c = (r == n0 || r == n1) && c >= nc0 && c <= nc1 || (c == nc0 || c == nc1) && r >= n0 && r <= n1
      -- perimeter cells only: without colour the bar tracks inside are rule-role "─" too
      novBorder = [ x | x@(r, c, _) <- roleCells RRule "─│┌┐└┘" b, onEdge r c ]
  length novBorder @?= 2 * (nc1 - nc0 + 1) + 2 * (n1 - n0 - 1)
  assertBool "Dec dashed border" (length (roleCells RRule "┄┆" b) > 10)

unit_gridFutureAscii :: Assertion
unit_gridFutureAscii = do
  b <- yearBody ["-Y", "--ascii"] 140 False
  assertBool "ASCII dashed border" (length (roleCells RRule ":" b) >= 2)
  assertBool "ASCII borders" (not (T.any (`elem` ("─│┌┐└┘┄┆━" :: String)) (plainText b)))

-- | The cursor is the month card's border, never a highlighted label (S4);
-- it wins over the current-month accent and the overspent tint.
unit_cursorBorder :: Assertion
unit_cursorBorder = do
  forM_ [(["-Y"], "Nov"), (["-Y", "-p", "2026-08"], "Aug"), (["-Y", "-p", "2026-02"], "Feb")] $ \(args, mon) -> do
    b <- yearBody args 140 True
    let cs = cursorCells b
        ((r0, c0), (r1, c1)) = cardRect mon b
    assertBool (T.unpack mon ++ ": cursor border drawn") (not (null cs))
    assertBool ("border glyphs only: " ++ show cs) (all (\(_, _, ch) -> ch `elem` ("─│┌┐└┘" :: String)) cs)
    assertBool (T.unpack mon ++ ": the card's box") (all (\(r, c, _) -> r >= r0 && r <= r1 && c >= c0 && c <= c1) cs)
    length cs @?= 2 * (c1 - c0 + 1) + 2 * (r1 - r0 - 1)
    -- the only reverse video is the open month's "to 6th" chip
    assertBool "no inverse label" ([ ch | (ch, s) <- concat (cellStyles b), stInverse s ] `elem` ["", " to 6th "])
    assertBool "no accent border anywhere" (null (roleCells RCursor "─│┌┐└┘" b))
  b <- yearBody ["-Y"] 140 False
  cursorCells b @?= []

-- | The cursor row is the cursor card's name row.
unit_gridCursorRow :: Assertion
unit_gridCursorRow = do
  env <- envFor ["-Y", "-p", "2026-08"]
  let ctx = fromJust (mkRenderCtx env 140 True)
      d = yearDoc ctx (yearModel env)
      ls = T.lines (plainText (vdBody d))
  r <- maybe (assertFailure "no cursor row") pure (vdCursorRow d)
  assertBool "Aug on the cursor row" ("│ Aug " `T.isInfixOf` (ls !! r))

-- | At 140 columns "avg … / full month" stays on one line.
unit_kpiSubLine :: Assertion
unit_kpiSubLine = do
  t <- plainText <$> yearRender ["-Y"] 140
  assertBool "one line" (any ("avg 2,191,600 / full month" `T.isInfixOf`) (T.lines t))
