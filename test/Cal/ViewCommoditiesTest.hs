module Cal.ViewCommoditiesTest where

import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit

import Cal.Doc
import Cal.Model.Commodities (commoditiesModel)
import Cal.Types (Cursor (..), defaultCursor)
import Cal.View.Commodities (commoditiesDoc)
import Cal.View.Types
import Support (edgePath, envFor)

oct, edge :: [String]
oct = ["--commodities", "-p", "2026-10"]
edge = oct ++ ["-f", edgePath]

vooCursor :: Cursor
vooCursor = Cursor 0 6

render :: [String] -> Cursor -> Int -> Bool -> IO (ViewDoc, Block)
render args cur w interactive = do
  env <- envFor args
  let ctx = fromJust (mkRenderCtx env w interactive)
      d = commoditiesDoc ctx (commoditiesModel env cur)
  pure (d, printBlock ctx d)

variants :: [(String, [String], Cursor, Int)]
variants =
  [ ("commodities-2026-10-" ++ show w, oct, defaultCursor, w) | w <- [140, 120, 100, 80] ]
  ++ [ ("commodities-2026-10-voo-140", oct, vooCursor, 140)
     , ("commodities-2026-10-edge-140", edge, defaultCursor, 140) ]

utf8 :: Text -> BL.ByteString
utf8 t = BL.fromStrict (TE.encodeUtf8 (t <> "\n"))

test_goldens :: TestTree
test_goldens = testGroup "goldens"
  [ goldenVsString name ("test/golden/" ++ name ++ ".txt") (utf8 . plainText . snd <$> render args cur w False)
  | (name, args, cur, w) <- variants ]

unit_widths :: Assertion
unit_widths = mapM_ check (variants ++ extra)
  where
    extra = [ ("oct-" ++ show w, oct, vooCursor, w) | w <- [80, 95, 100, 119, 120, 139, 200] ]
      ++ [ ("edge-" ++ show w, edge, defaultCursor, w) | w <- [80, 100, 120] ]
      ++ [ ("ascii-" ++ show w, oct ++ ["--ascii"], vooCursor, w) | w <- [80, 140] ]
      ++ [ ("native-" ++ show w, oct ++ ["--native"], vooCursor, w) | w <- [80, 140] ]
      ++ [ (i ++ "-" ++ show w, ["--commodities", i, "-p", p], vooCursor, w)
         | (i, p) <- [("-Y", "2026"), ("-Q", "2026q4"), ("-W", "2026-10-26")], w <- [80, 140] ]
    check (name, args, cur, w) = do
      b <- snd <$> render args cur w False
      let wide = [ l | l <- T.lines (plainText b), textW l > w ]
      assertBool (name ++ " has lines wider than " ++ show w ++ ": " ++ show wide) (null wide)
      assertBool (name ++ " reports width " ++ show (bWidth b)) (bWidth b <= w)

-- | Text of the boxes with borders and wrapping removed.
unwrapped :: Text -> Text
unwrapped t = T.unwords (concatMap (T.words . T.filter (`notElem` ("│┌┐└┘─" :: String))) (T.lines t))

hasLine :: Text -> [Text] -> Bool
hasLine t frags = any (\l -> all (`T.isInfixOf` l) frags) (T.lines t)

unit_checks :: Assertion
unit_checks = do
  t <- plainText . snd <$> render oct defaultCursor 140 False
  assertBool "title" ("Commodities  October 2026 · held at 31 Oct" `T.isInfixOf` t)
  assertBool "headings" (hasLine t ["commodity", "held", "this month", "price ₦", "Δ mo", "12 months", "value ₦", "share", "unreal. gain", "price source"])
  assertBool "groups" (hasLine t ["CURRENCIES"] && hasLine t ["SECURITIES"])
  assertBool "base row" (hasLine t ["₦ NGN", "1 — base"])
  assertBool "mtnn" (hasLine t ["MTNN", "630,000", "P · 31 Oct"])
  assertBool "voo" (hasLine t ["VOO", "2,604,000", "via USD · 31 Oct"])
  assertBool "usd inferred" (hasLine t ["$ USD", "$2,500.00", "inferred · 31 Oct"])
  assertBool "aed stale" (hasLine t ["AED •", "stale · 14 Aug (78 d)"])
  assertBool "total" (hasLine t ["total", "8 commodities", "0 unpriced", "16,316,000", "1 stale price"])
  assertBool "explainer title" ("How these numbers are made" `T.isInfixOf` t)
  mapM_ (\l -> assertBool ("explainer: " ++ T.unpack l) (l `T.isInfixOf` unwrapped t))
    [ "· A commodity is anything the journal counts: currencies, shares, units. Each one is valued in the base through any chain of prices (VOO → USD → NGN)."
    , "· Prices come from P directives (e.g. a daily price feed) first. Otherwise they're inferred from transaction costs (@ / @@)."
    , "· Unrealized gain = value now − what was paid (the @ cost)."
    , "· A security priced only by inference moves only when you trade it. Add P directives to follow the market." ]
  assertBool "stale note" ("• AED has no price since 14 Aug, so a 78-day-old rate is used." `T.isInfixOf` unwrapped t)
  assertBool "legend" ("share = part of total value held" `T.isInfixOf` t)
  assertBool "no key hints in print" (not ("value in this" `T.isInfixOf` t))
  t120 <- plainText . snd <$> render oct defaultCursor 120 False
  assertBool "no source at 120" (not ("price source" `T.isInfixOf` t120) && hasLine t120 ["12 months", "unreal. gain"])
  t80 <- plainText . snd <$> render oct defaultCursor 80 False
  assertBool "drops at 80" (not (hasLine t80 ["commodity", "12 months"]) && not ("unreal. gain" `T.isInfixOf` t80))
  assertBool "mtnn 80" (hasLine t80 ["MTNN", "630,000"])

unit_voo :: Assertion
unit_voo = do
  t <- plainText . snd <$> render oct vooCursor 140 False
  assertBool "detail title" (hasLine t ["VOO in ₦ · per share · 12 months", "710,400 → 868,000 · +22.2%"])
  assertBool "split" (hasLine t ["share price in $ +16.7%", "× $ → ₦ +4.7%", "= in ₦ +22.2%"])
  assertBool "x axis" (hasLine t ["N", "D", "J", "F", "M", "A", "S", "O"])

unit_edge :: Assertion
unit_edge = do
  t <- plainText . snd <$> render edge defaultCursor 140 False
  assertBool "btc unpriced" (hasLine t ["BTC", "no price"])
  -- the BTC row's cells, split on runs of two or more spaces
  let btc = [ cells l | l <- T.lines t, "BTC " `T.isPrefixOf` l ]
      cells l = filter (not . T.null) (map T.strip (T.splitOn "  " l))
  btc @?= [["BTC", "−0.01 BTC", "−0.01 BTC", "—", "—", "············", "—", "—", "—", "no price"]]
  assertBool "total unpriced" (hasLine t ["total", "1 unpriced", "0 stale prices"])

unit_ascii :: Assertion
unit_ascii = do
  t <- plainText . snd <$> render (oct ++ ["--ascii"]) vooCursor 140 False
  let bad = [ c | c <- T.unpack t, fromEnum c > 127, c `notElem` ("₦$€₹—" :: String) ]
  assertBool ("non-ASCII glyphs: " ++ show (take 20 bad)) (null bad)

unit_cursor :: Assertion
unit_cursor = do
  (d, _) <- render oct vooCursor 140 True
  case vdCursorRow d of
    Nothing -> assertFailure "cursor row not set when interactive"
    Just r -> assertBool "cursor on VOO" ("VOO" `T.isInfixOf` (T.lines (plainText (vdBody d)) !! r))
  (d', _) <- render oct vooCursor 140 False
  vdCursorRow d' @?= Nothing
