module Cal.ViewYearTest where

import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit

import Cal.Doc
import Cal.Model.Year (yearModel)
import Cal.View.Types
import Cal.View.Year (yearDoc)
import Support (envFor)

yearRender :: [String] -> Int -> IO Block
yearRender extra w = do
  env <- envFor extra
  let ctx = fromJust (mkRenderCtx env w False)
  pure (printBlock (yearDoc ctx (yearModel env)))

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
  assertBool "Dec forecast" ("forecast Household payroll $1,200.00 ≈ −1,860,000" `T.isInfixOf` t)
