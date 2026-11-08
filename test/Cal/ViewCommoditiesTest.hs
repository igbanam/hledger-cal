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
import Cal.Theme (Role (..))
import Support (cellStyles, cursorCells, edgePath, envFor)

oct, edge :: [String]
oct = ["--commodities", "-p", "2026-10"]
edge = oct ++ ["-f", edgePath]

vooCursor :: Cursor
vooCursor = Cursor 0 (Just 6)

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

-- | Text of the boxes with borders and wrapping removed; where two boxes sit
-- side by side, only the right one (the explainer) is kept.
unwrapped :: Text -> Text
unwrapped t = T.unwords (concatMap (T.words . T.filter (`notElem` ("│┌┐└┘─" :: String)) . rightBox) (T.lines t))
  where rightBox l = last (T.splitOn "│  │" l)

hasLine :: Text -> [Text] -> Bool
hasLine t frags = any (\l -> all (`T.isInfixOf` l) frags) (T.lines t)

unit_checks :: Assertion
unit_checks = do
  t <- plainText . snd <$> render oct defaultCursor 140 False
  assertBool "title" ("Commodities  October 2026 · held at 31 Oct" `T.isInfixOf` t)
  assertBool "headings" (hasLine t ["commodity", "held", "this month", "price ₦", "Δ mo", "12 months", "value ₦", "share", "unreal. gain", "price source"])
  assertBool "groups" (hasLine t ["CURRENCIES"] && hasLine t ["SECURITIES"])
  assertBool "base row" (hasLine t ["₦ NGN base", " 1 ", " — ", " base "])
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
  -- the BTC row's cells inside the table box, split on runs of two or more spaces
  let btc = [ cells l | l <- T.lines t, "│ BTC  " `T.isPrefixOf` l ]
      cells l = filter (not . T.null) (map T.strip (T.splitOn "  " (T.filter (/= '│') l)))
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


-- | The table: one box, full width, at every tier, with a rule between rows.
unit_tableBox :: Assertion
unit_tableBox = mapM_ check [200, 140, 120, 100, 80]
  where
    check w = do
      (d, _) <- render oct vooCursor w False
      let ls = T.lines (plainText (vdBody d))
          tag = "width " ++ show w ++ ": "
      let ends l = (T.take 1 l, T.takeEnd 1 l)
      map ends (take 1 ls) @?= [("┌", "┐")]
      map ends (take 1 (reverse ls)) @?= [("└", "┘")]
      assertBool (tag ++ "edges") (all (\l -> ends l `elem` [("│", "│"), ("├", "┤")]) (drop 1 (take (length ls - 1) ls)))
      assertBool (tag ++ "full width") (all ((== w) . textW) ls)
      -- every row is closed by a rule, NGN's and USD's included
      let rowAt k = [ i | (i, l) <- zip [0 :: Int ..] ls, k `T.isPrefixOf` T.drop 2 l ]
      case (rowAt "₦ NGN", rowAt "$ USD", rowAt "total") of
        ([n], [u], [t]) -> do
          u @?= n + 2
          assertBool (tag ++ "rule between") ("├" `T.isPrefixOf` (ls !! (n + 1)))
          assertBool (tag ++ "rule above total") ("├" `T.isPrefixOf` (ls !! (t - 1)))
        other -> assertFailure (tag ++ "rows not found: " ++ show other)

-- | Group headers sit on the panel tint, border to border; data rows do not.
unit_groupTint :: Assertion
unit_groupTint = do
  (d, _) <- render oct vooCursor 140 False
  let rowsS = cellStyles (vdBody d)
      text r = T.pack (map fst r)
      inner r = drop 1 (take (length r - 1) r)
      grp = [ r | r <- rowsS, any (`T.isInfixOf` text r) ["CURRENCIES", "SECURITIES"] ]
      usd = [ r | r <- rowsS, "$ USD" `T.isInfixOf` text r ]
  length grp @?= 2
  assertBool "group cells tinted" (all (all ((== Just (RPanel, 100)) . stBg . snd) . inner) grp)
  assertBool "group label dim" (all (\r -> all ((== RDim) . stRole . snd) (inner r)) grp)
  assertBool "data rows untinted" (all (all ((== Nothing) . stBg . snd)) usd)

unit_rowCells :: Assertion
unit_rowCells = do
  (d, _) <- render oct vooCursor 140 False
  let ls = T.lines (plainText (vdBody d))
      after k = case dropWhile (not . (k `T.isInfixOf`)) ls of
        (_ : n : _) -> n
        _ -> ""
  assertBool "base tag" (hasLine (T.unlines ls) ["₦ NGN base"])
  assertBool "voo quote symbol and shares" (hasLine (T.unlines ls) ["VOO $", "3 sh", "868,000.00"])
  assertBool "mtnn quote symbol and shares" (hasLine (T.unlines ls) ["MTNN ₦", "1,500 sh", "+500 sh"])
  -- the native price, alone on VOO's second line, under the base price
  T.words (T.filter (/= '│') (after "VOO $")) @?= ["$560.00"]
  let col k l = T.length (fst (T.breakOn k l))
      vooL = T.concat (take 1 (filter ("VOO $" `T.isInfixOf`) ls))
  col "$560.00" (after "VOO $") + T.length "$560.00" @?= col "868,000.00" vooL + T.length "868,000.00"
  -- currencies keep their symbol; only one line each
  assertBool "usd held" (hasLine (T.unlines ls) ["$ USD", "$2,500.00"])
  assertBool "usd one line" ("├" `T.isPrefixOf` after "$ USD")

-- | A thin bar before the share, scaled to the largest share.
unit_shareBar :: Assertion
unit_shareBar = do
  t <- plainText . vdBody . fst <$> render oct vooCursor 140 False
  assertBool "largest share fills the bar" (hasLine t ["₦ NGN", "━━━━━━ 49.4%"])
  assertBool "half the largest, half the bar" (hasLine t ["$ USD", "━━━─── 23.7%"])
  assertBool "small but non-zero" (hasLine t ["KD KWD", "━─────  0.6%"])
  t80 <- plainText . vdBody . fst <$> render oct vooCursor 80 False
  assertBool "no bar where it would cost a column" (not ("━" `T.isInfixOf` t80) && hasLine t80 ["$ USD", "+$100.00", "23.7%"])

-- | A gently rising price reads as a staircase (min–max per row).
unit_sparkStaircase :: Assertion
unit_sparkStaircase = do
  t <- plainText . vdBody . fst <$> render oct vooCursor 140 False
  let usd = T.concat (take 1 (filter ("$ USD" `T.isInfixOf`) (T.lines t)))
  assertBool ("USD staircase: " ++ T.unpack usd) ("··▁▁▂▃▃▃▄▅▅█" `T.isInfixOf` usd)

-- | S4: the cursor row is boxed in the accent at the table's rules; no label is reversed.
unit_cursorBorder :: Assertion
unit_cursorBorder = mapM_ check [140, 80]
  where
    check w = do
      (d, _) <- render oct vooCursor w True
      let body = vdBody d
          cs = cursorCells body
          ls = T.lines (plainText body)
          rows = [ r | (r, _, _) <- cs ]
          r0 = minimum rows
          r1 = maximum rows
          tag = "width " ++ show w ++ ": "
      assertBool (tag ++ "drawn") (not (null cs))
      assertBool (tag ++ "border glyphs only") (all (\(_, _, ch) -> ch `elem` ("─│┌┐└┘├┤" :: String)) cs)
      assertBool (tag ++ "VOO inside") ("VOO $" `T.isInfixOf` (ls !! (r0 + 1)))
      -- both of VOO's lines are inside: rule, VOO, $560.00, rule
      r1 - r0 @?= 3
      vdCursorRow d @?= Just (r0 + 1)
      vdCursorLast d @?= Just r1
      length cs @?= 2 * w + 2 * (r1 - r0 - 1)
      assertBool (tag ++ "no reversed cell") (not (any (stInverse . snd) (concat (cellStyles body))))
      (d', _) <- render oct vooCursor w False
      cursorCells (vdBody d') @?= []

-- | The detail chart: a bar per month on three y labels, month initials
-- under the bars, the last bar highlighted, the range on the title row and
-- the decomposition foot for a price via another currency.
unit_detailChart :: Assertion
unit_detailChart = do
  (d, _) <- render (oct ++ ["--color=always"]) vooCursor 140 False
  let panel = vdBottom d
      ls = T.lines (plainText panel)
      -- the detail box is the left one; its inner text
      inner = [ T.drop 2 (fst (T.breakOn "│  │" l)) | l <- ls ]
  assertBool "title and range" (hasLine (T.unlines inner) ["VOO in ₦ · per share · 12 months", "710,400 → 868,000 · +22.2%"])
  let yLabels = [ T.strip (fst (T.breakOn "│" l)) | l <- inner, "│" `T.isInfixOf` l ]
  filter (not . T.null) yLabels @?= ["870k", "785k", "700k"]
  -- the highest bar reaches the top row; January, the lowest, still shows
  let barRows = [ T.drop 1 (snd (T.breakOn "│" l)) | l <- inner, "│" `T.isInfixOf` l ]
      blocks = "█▁▂▃▄▅▆▇" :: String
      isBlock = (`elem` blocks)
      runs t = length (filter ((== '#') . T.head) (T.group (T.map (\ch -> if isBlock ch then '#' else ' ') t)))
  assertBool "top row not empty" (T.any isBlock (T.concat (take 1 barRows)))
  -- every priced month (Jan to Oct) shows on the floor row
  map runs (take 1 (reverse barRows)) @?= [10]
  assertBool "initials" (any ((== T.words "N D J F M A M J J A S O") . T.words) inner)
  assertBool "decomposition" (hasLine (T.unlines inner) ["share price in $ +16.7%", "× $ → ₦ +4.7%", "= in ₦ +22.2%"])
  -- bar cells: the rightmost bar alone is in the text colour, the rest faded
  -- the detail box ends where the two boxes meet
  let detailW = maximum (0 : [ T.length (fst (T.breakOn "│  │" l)) + 1 | l <- ls, "│  │" `T.isInfixOf` l ])
      bars = [ (col, stRole s) | r <- cellStyles panel, (col, (ch, s)) <- zip [0 :: Int ..] r
                               , ch `elem` ("█▁▂▃▄▅▆▇" :: String), col < detailW ]
      lit = [ c | (c, RText) <- bars ]
      faded = [ c | (c, RFaded) <- bars ]
  assertBool "one lit bar" (not (null lit) && maximum lit - minimum lit < 6)
  assertBool "lit bar is the last" (not (null faded) && minimum lit > maximum faded)
  assertBool "only text and faded bars" (all ((`elem` [RText, RFaded]) . snd) bars)
  -- the default (print) selection: USD, priced directly, so no decomposition
  t <- plainText . snd <$> render oct defaultCursor 140 False
  assertBool "usd chart" (hasLine t ["USD in ₦ · per unit · 12 months", "1,480 → 1,550 · +4.7%"] && hasLine t ["1,550 │"])
  assertBool "no decomposition for usd" (not ("share price in" `T.isInfixOf` t))

-- | Held and this-month amounts share one unit per row: a whole-share
-- security in sh, a unit: tag's word, else the commodity itself.
unit_units :: Assertion
unit_units = do
  t <- plainText . snd <$> render ["--commodities", "-p", "2026-10", "-f", "test/fixtures/units.journal"] defaultCursor 140 False
  assertBool "whole shares" (hasLine t ["ACME ₦", "6 sh", "+6 sh"])
  assertBool "unit: tag" (hasLine t ["GOLD ₦", "2.000 oz", "+2.000 oz"])
  assertBool "fractional, own commodity" (hasLine t ["BTC ₦", "0.0100 BTC", "+0.0100 BTC"])
  assertBool "chart per tagged unit" (hasLine t ["GOLD in ₦ · per oz · 12 months"])
  -- the design fixture's whole-share securities
  d <- plainText . snd <$> render oct defaultCursor 140 False
  assertBool "voo in sh" (hasLine d ["VOO $", "3 sh"])

-- | The columns fill the box: spread over the gaps, the slack leaves less
-- than one cell per gap free after the widest row at every tier.
unit_tableFills :: Assertion
unit_tableFills = mapM_ check [200, 140, 120, 100, 80]
  where
    check w = do
      (d, _) <- render oct vooCursor w False
      let ls = [ l | l <- T.lines (plainText (vdBody d)), "│ " `T.isPrefixOf` l, not ("CURRENCIES" `T.isInfixOf` l || "SECURITIES" `T.isInfixOf` l) ]
          free l = T.length (T.takeWhileEnd (== ' ') (T.dropEnd 1 l)) - 1
          least = minimum (map free ls)
      assertBool ("width " ++ show w ++ ": " ++ show least ++ " cells free after the widest row") (least < 9)
