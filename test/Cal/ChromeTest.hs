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
import Cal.Model.Common (Env (..), kpisFor)
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
