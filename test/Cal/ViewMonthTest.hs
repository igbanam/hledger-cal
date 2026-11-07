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
import Cal.Types (Settings (..))
import Cal.View.Month (monthDoc)
import Cal.View.Types
import Support (edgePath, envFor)

monthRender :: [String] -> Int -> IO Block
monthRender args w = do
  env <- envFor args
  let ctx = fromJust (mkRenderCtx env w False)
  pure (printBlock (monthDoc ctx (monthModel env)))

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
  assertBool "30 Oct title" ("Fri 30 Oct 2026 · 1 txn · 2 postings · USD" `T.isInfixOf` t30)
  assertBool "30 Oct rate" (hasLine t30 ["expenses:payroll:security", "$700.00", "@ ₦1,550.00", "−1,085,000"])
  assertBool "rates title" ("Rates · 31 Oct" `T.isInfixOf` t)
  assertBool "rates heading" ("₦ per 1 unit · Δ month" `T.isInfixOf` t)
  assertBool "stale AED" (hasLine t ["AED •", "stale · 14 Aug"])
  assertBool "legend" ("▢ day under cursor · grey = native amount" `T.isInfixOf` t)

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
      t = plainText (printBlock (monthDoc ctx (monthModel env)))
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
