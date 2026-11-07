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
import Support (envFor)

quarterRender :: [String] -> Int -> IO Block
quarterRender extra w = do
  env <- envFor extra
  let ctx = fromJust (mkRenderCtx env w False)
  pure (printBlock (quarterDoc ctx (quarterModel env)))

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
