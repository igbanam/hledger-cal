module Cal.ViewQuarterTest where

import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit

import Cal.Doc
import Cal.Model.Quarter (quarterModel)
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
