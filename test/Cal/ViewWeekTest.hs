module Cal.ViewWeekTest where

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
import Cal.Types (defaultCursor)
import Cal.View.Types
import Cal.View.Week (weekDoc)
import Support (envFor)

week44, week53 :: [String]
week44 = ["-W", "-p", "2026-10-28"]
week53 = ["-W", "-p", "2026-12-30"]

weekRender :: [String] -> Int -> Bool -> IO (ViewDoc, Block)
weekRender args w interactive = do
  env <- envFor args
  let ctx = fromJust (mkRenderCtx env w interactive)
      d = weekDoc ctx (weekModel env defaultCursor)
  pure (d, printBlock d)

variants :: [(String, [String], Int)]
variants =
  [ ("week-2026-44-" ++ show w, week44, w) | w <- [140, 120, 100, 80] ]
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
    extra = [ ("w44-" ++ show w, week44, w) | w <- [95, 119, 139, 200] ]
      ++ [ ("ascii-" ++ show w, week44 ++ ["--ascii"], w) | w <- [80, 140] ]
      ++ [ ("native-" ++ show w, week44 ++ ["--native"], w) | w <- [80, 140] ]
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
  assertBool "tue 27 location" (hasLine t ["expenses:fuel", "Port Harcourt", "₦30,500"])
  assertBool "tue 27 row" (hasLine t ["Tue 27", "−30,500"])
  assertBool "payroll share" ("96% of it is payroll" `T.isInfixOf` t)
  assertBool "no income" ("none this week" `T.isInfixOf` t)
  assertBool "empty day" (hasLine t ["Mon 26", "0", "·"])
  assertBool "sun" (hasLine t ["Sun 1 Nov"])
  assertBool "fx line" (hasLine t ["    FX effect −12,000 (txn-date rate 1,540.00)"])
  assertBool "rate" (hasLine t ["$700.00", "@1,550", "−1,085,000"])
  assertBool "activity" ("3 of 7 days" `T.isInfixOf` t && "2 currencies" `T.isInfixOf` t)
  t80 <- plainText . snd <$> weekRender week44 80 False
  assertBool "activity whole" ("3 of 7 days · 2 currencies" `T.isInfixOf` t80)
  assertBool "legend" ("bar = day's spend · grey = native amount @ rate" `T.isInfixOf` t)
  t53 <- plainText . snd <$> weekRender week53 140 False
  assertBool "week 53 title" ("Week 53" `T.isInfixOf` t53)

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
    Just r -> assertBool "day label" ("Wed 28" `T.isInfixOf` (T.lines (plainText (vdBody d2)) !! r))

unit_asciiKeys :: Assertion
unit_asciiKeys = do
  (d, _) <- weekRender (week44 ++ ["--ascii"]) 140 False
  assertBool "transaction hint kept" ("transaction" `elem` map snd (vdKeys d))
