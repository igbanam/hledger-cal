module Cal.ViewRegisterTest where

import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (fromGregorian)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit

import Cal.Doc
import Cal.Model.Common (Env (..))
import Cal.Model.Register (registerRows)
import Cal.Period (intervalSpan)
import Cal.Types
import Cal.View.Register
import Cal.View.Types
import Support (edgePath, envFor)

pane :: [String] -> Day -> Int -> Int -> Int -> IO Block
pane extra day w height scroll = do
  env <- envFor (["-M", "-p", "2026-10"] ++ extra)
  let ctx = fromJust (mkRenderCtx env w False)
      sp = intervalSpan IMonth (sFocus (envSettings env))
  pure (registerPane ctx day height scroll (registerRows env sp day))

d30, d10 :: Day
d30 = fromGregorian 2026 10 30
d10 = fromGregorian 2026 10 10

-- | (name, extra args, day, width)
variants :: [(String, [String], Day, Int)]
variants =
  [ ("register-2026-10-30-140", [], d30, 140)
  , ("register-2026-10-30-80", [], d30, 80)
  , ("register-edge-80", ["-f", edgePath], d10, 80) ]

utf8 :: Text -> BL.ByteString
utf8 t = BL.fromStrict (TE.encodeUtf8 (t <> "\n"))

test_goldens :: TestTree
test_goldens = testGroup "goldens"
  [ goldenVsString name ("test/golden/" ++ name ++ ".txt") (utf8 . plainText <$> pane extra day w 12 0)
  | (name, extra, day, w) <- variants ]

unit_widths :: Assertion
unit_widths = mapM_ check variants
  where
    check (name, extra, day, w) = do
      b <- pane extra day w 12 0
      let wide = [ l | l <- T.lines (plainText b), textW l > w ]
      assertBool (name ++ " has lines wider than " ++ show w ++ ": " ++ show wide) (null wide)
      assertEqual (name ++ " height") 12 (length (bLines b))

unit_scroll :: Assertion
unit_scroll = do
  a <- pane [] d30 140 12 0
  b <- pane [] d30 140 12 1
  let ls x = T.lines (plainText x)
  assertEqual "same height" 12 (length (bLines b))
  assertBool "first row dropped" (ls a !! 1 /= ls b !! 1)
  assertEqual "second row now first" (ls a !! 2) (ls b !! 1)
