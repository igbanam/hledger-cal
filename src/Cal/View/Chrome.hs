-- | Chrome shared by every view: command line, header, currency bar, KPI
-- cards and the key-hint footer. Pure.
module Cal.View.Chrome
  ( topChrome
  , incomeCard, expensesCard, netCard, fxCard, activityCard
  , cardRow, mixBlock, footerBlock
  ) where

import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Cal.Cli (commandLine)
import Cal.Currency (chipLabel, codeOf, symbolOf)
import Cal.Doc
import Cal.Format (Fmt (..), fmtDeltaPct, fmtPct, fmtQty, fmtSigned)
import Cal.Glyphs (BoxChars (..), Glyphs (..))
import Cal.Model.Common (Activity (..), FxCause (..), Kpis (..), Mix (..))
import Cal.Model.Shared (CurrencyBar (..))
import Cal.Theme (Role (..))
import Cal.Types
import Cal.Valuation (baseQty)
import Cal.View.Types

-- * Small helpers

dim, plain :: Style
dim = st RDim
plain = st RText

segsW :: [Seg] -> Int
segsW ss = sum [textW t | Seg _ t <- ss]

blank :: Block
blank = Block 0 [Line []]

-- | Greedy flow of items into lines of at most @w@ columns, @sep@ between
-- items on the same line. An item wider than @w@ sits alone on its line.
flow :: Int -> [Seg] -> [[Seg]] -> [[Seg]]
flow _ _ [] = []
flow w sep (x0 : xs0) = go x0 (segsW x0) xs0
  where
    sepW = segsW sep
    go cur _ [] = [cur]
    go cur cw (y : ys)
      | cw + sepW + yW <= w = go (cur ++ sep ++ y) (cw + sepW + yW) ys
      | otherwise = cur : go y yW ys
      where yW = segsW y

linesB :: [[Seg]] -> Block
linesB = vcat . map line

-- | Word-wrapped text in one style; over-long words are truncated.
wrapText :: Glyphs -> Style -> Int -> Text -> Block
wrapText g s w t = linesB
  [ [Seg s (T.concat [x | Seg _ x <- ss])] | ss <- flow w [Seg s " "] [[Seg s (truncEnd g w wd)] | wd <- T.words t] ]

-- * Top chrome

-- | Command line, header, currency bar, then the given blocks (usually a 'cardRow').
topChrome :: RenderCtx -> Text -> CurrencyBar -> [Block] -> Block
topChrome ctx title cb cards = padTo w $ vcat $
  [cmd, blank, header ctx title, currencyBarBlock ctx cb] ++ (if null cards then [] else blank : cards)
  where
    w = rcWidth ctx
    cmd = line [Seg (st RInc) "$", Seg dim (" " <> commandLine (rcSettings ctx))]

header :: RenderCtx -> Text -> Block
header ctx title
  | gap >= 1 = line (left ++ [Seg plain (T.replicate gap " ")] ++ tabs)
  | otherwise = padTo w (line left)
  where
    w = rcWidth ctx
    g = rcGlyphs ctx
    s = rcSettings ctx
    tabs =
      [ if f == sFace s then Seg (inverse (bold plain)) t else Seg dim t
      | (f, n) <- [ (FaceYear, "year"), (FaceQuarter, "quarter"), (FaceMonth, "month")
                  , (FaceWeek, "week"), (FaceCommodities, "commodities") ]
      , let t = " " <> n <> " " ]
    filt = if null (sQueryArgs s) then "all accounts" else T.unwords (sQueryArgs s)
    fixed = [Seg dim (gLeft g), Seg plain " ", Seg (bold plain) title, Seg plain " ", Seg dim (gRight g)]
    room = w - segsW tabs - 1 - segsW fixed - 2
    left = fixed ++ (if room > 0 then [Seg plain "  ", Seg dim (truncEnd g room filt)] else [])
    gap = w - segsW left - segsW tabs

currencyBarBlock :: RenderCtx -> CurrencyBar -> Block
currencyBarBlock ctx cb = boxed g Nothing w (hcat 0 [str plain " ", padTo cw (linesB content)])
  where
    w = rcWidth ctx
    g = rcGlyphs ctx
    cw = w - 4
    selSt = inverse (bold plain)
    chip sel label stale =
      let sty = if sel then selSt else dim
          warn = if sel then inverse (st RWarn) else st RWarn
      in [Seg sty (" " <> label)] ++ (if stale then [Seg sty " ", Seg warn (gStale g)] else []) ++ [Seg sty " "]
    chips =
      [Seg dim "valued in", Seg plain " "]
        : [ chip (cbValuation cb == ValuedIn c) (chipLabel c) stale | (c, stale) <- cbChoices cb ]
        ++ [chip (cbValuation cb == Native) "native" False]
    note = case cbValuation cb of
      ValuedIn b | b /= cbDefault cb ->
        [Seg (st RCursor) ("switched" <> gSep g <> "default is " <> codeOf (cbDefault cb))]
      _ | T.null (cbDefault cb) -> []
        | otherwise -> [Seg dim (codeOf (cbDefault cb) <> " = default" <> gSep g <> source)]
    source = case cbSource cb of
      BaseFirstSeen -> "first in journal"
      BaseFromD -> "D directive"
      BaseFromX -> "-X"
    rates = Seg dim "rates at" : Seg plain " "
      : [ Seg (if r == cbRateAt cb then bold plain else dim) (" " <> n <> " ")
        | (r, n) <- [(AtPeriodEnd, "period end"), (AtTxnDate, "txn date"), (AtToday, "today")] ]
    -- the model joins sources with " · "; use the glyph set's separator
    priceNote = maybe [] (\t -> [Seg dim (T.replace " · " (gSep g) t)]) (cbPriceNote cb)
    two = [Seg plain "  "]
    -- chips (wrapped), then the note on the last chip line if it fits
    chipLines = flow cw [] chips
    leftLines
      | null note = chipLines
      | segsW (last chipLines) + 2 + segsW note <= cw = init chipLines ++ [last chipLines ++ two ++ note]
      | otherwise = chipLines ++ [note]
    rightOne = rates ++ (if null priceNote then [] else two ++ priceNote)
    rightLines
      | segsW rightOne <= cw = [rightOne]
      | otherwise = rates : [priceNote | not (null priceNote)]
    lastW = segsW (last leftLines)
    -- the rates group sits on the right, on the last left line when it fits
    pushR r = Seg plain (T.replicate (max 0 (cw - segsW r)) " ") : r
    content = case rightLines of
      [r] | rcTier ctx /= Tier80, lastW + 2 + segsW r <= cw ->
        init leftLines ++ [last leftLines ++ [Seg plain (T.replicate (cw - lastW - segsW r) " ")] ++ r]
      _ -> leftLines ++ map pushR rightLines

-- * Cards

showMix :: RenderCtx -> Bool
showMix ctx = rcTier ctx >= Tier120

card :: RenderCtx -> Int -> Text -> Seg -> [Block] -> Block
card ctx w label (Seg s v) rest =
  vcat ([str dim label, str s (truncEnd (rcGlyphs ctx) w v)] ++ rest)

sub :: RenderCtx -> Int -> Text -> Block
sub ctx = wrapText (rcGlyphs ctx) dim

-- | INCOME: the value, the change against the previous period, then the mix.
incomeCard :: RenderCtx -> Int -> Text -> Kpis -> Block
incomeCard ctx w prev k = card ctx w "INCOME" (Seg (bold (st RInc)) (fmtSigned f (kIncome k))) $
  [ sub ctx w (fmtSigned f (kIncome k - pIn) <> " vs " <> prev) | Just (pIn, _) <- [kPrev k], prev /= "" ]
  ++ [ mixBlock ctx KIncome w m | showMix ctx, Just m <- [kIncomeMix k] ]
  where f = rcFmt ctx

-- | EXPENSES: the value, the change against the previous period with its
-- percentage, then the mix.
expensesCard :: RenderCtx -> Int -> Text -> Kpis -> Block
expensesCard ctx w prev k = card ctx w "EXPENSES" (Seg (bold (st RExp)) (fmtSigned f (negate e))) $
  [ sub ctx w (fmtSigned f (e - pOut) <> " vs " <> prev <> pct pOut) | Just (_, pOut) <- [kPrev k], prev /= "" ]
  ++ [ mixBlock ctx KExpense w m | showMix ctx, Just m <- [kExpenseMix k] ]
  where
    f = rcFmt ctx
    e = kExpenses k
    pct pOut = case fBase f of
      Just b | baseQty b pOut /= 0 ->
        let r = realToFrac (baseQty b e - baseQty b pOut) / realToFrac (baseQty b pOut) :: Double
        in " (" <> fmtDeltaPct f r <> ")"
      _ -> ""

netCard :: RenderCtx -> Int -> Kpis -> Block
netCard ctx w k = card ctx w "NET" (Seg (bold plain) (fmtSigned (rcFmt ctx) (kNet k)))
  [sub ctx w (maybe "—" fmtPct (kKept k) <> " of income kept")]

fxCard :: RenderCtx -> Int -> Kpis -> Block
fxCard ctx w k = card ctx w "FX EFFECT" value $
  sub ctx w "period-end vs txn-date rates"
    : [ sub ctx w ("mostly " <> codeOf c <> " " <> grp <> " (" <> fmtQty f amt <> ")")
      | Just (FxCause c grp amt) <- [kFxCause k], kFx k /= Just 0 ]
  where
    f = rcFmt ctx
    value = case kFx k of
      Nothing -> Seg (bold dim) "—"
      Just q -> Seg (bold (st (if q < 0 then RExp else if q > 0 then RInc else RText))) (fmtQty f q)

activityCard :: RenderCtx -> Int -> Kpis -> Block
activityCard ctx w k = card ctx w "ACTIVITY" (Seg (bold plain) (count n "txn" "txns"))
  [sub ctx w (count (length (aCommodities a)) "currency" "currencies" <> " used")]
  where
    a = kActivity k
    n = aTxns a
    count i one many = T.pack (show i) <> " " <> (if i == 1 then one else many)

-- | Cards side by side, split evenly over the width with ruled separators.
-- At 'Tier80' they wrap onto two rows.
cardRow :: RenderCtx -> [Int -> Block] -> Block
cardRow _ [] = emptyB
cardRow ctx fs = vcat (intersperse blank (map row rows))
  where
    w = rcWidth ctx
    n = length fs
    rows
      | rcTier ctx == Tier80 = let (a, b) = splitAt ((n + 1) `div` 2) fs in filter (not . null) [a, b]
      | otherwise = [fs]
    row cs =
      let k = length cs
          (q, r) = (w - 3 * (k - 1)) `divMod` k
          ws = [q + (if i < r then 1 else 0) | i <- [0 .. k - 1]]
          blocks = zipWith (\c cw -> padTo cw (c cw)) cs ws
          h = maximum (map (length . bLines) blocks)
          sep = Block 3 (replicate h (Line [Seg (st RRule) (" " <> T.singleton (bxV (gBox (rcGlyphs ctx))) <> " ")]))
      in hcat 0 (intersperse sep blocks)

-- | A mix bar @w@ cells wide, segments tinted by rank, and its legend.
mixBlock :: RenderCtx -> Kind -> Int -> Mix -> Block
mixBlock ctx kind w (Mix xs) = vcat [bar, linesB (flow w [Seg dim (gSep g)] legend)]
  where
    g = rcGlyphs ctx
    role = if kind == KIncome then RInc else RExp
    tints = [100, 60, 40, 25] ++ repeat 25
    rankCh i = case drop i (gMixRank g) of
      (c : _) -> c
      [] -> last (gFull g : gMixRank g)
    bounds = [min w (round (c * fromIntegral w)) | c <- scanl (+) 0 (map snd xs)] :: [Int]
    widths = zipWith (-) (drop 1 bounds) bounds
    bar = line [ Seg (tinted t (st role)) (T.replicate cells (T.singleton (rankCh i)))
               | (i, t, cells) <- zip3 [0 :: Int ..] tints widths, cells > 0 ]
    legend = [ [Seg dim (fromMaybe c (symbolOf c) <> " " <> fmtPct sh)] | (c, sh) <- xs ]

-- | Key hints on the left (replaced by the message, in the warning colour,
-- when there is one) and the legend on the right.
footerBlock :: RenderCtx -> [(Text, Text)] -> Block -> Block
footerBlock ctx keys legend
  | lw == 0 = padTo w (left w)
  | w - lw - 2 >= 40 = hcat 0 [padTo (w - lw) (left (w - lw - 2)), alignR lw legend]
  | otherwise = vcat [padTo w (left w), alignR w legend]
  where
    w = rcWidth ctx
    g = rcGlyphs ctx
    lw = min w (bWidth legend)
    left room = case rcMessage ctx of
      Just m -> wrapText g (st RWarn) room m
      Nothing -> linesB (flow room [Seg plain "  "]
        [ [Seg (inverse dim) (" " <> k <> " "), Seg dim (" " <> l)] | (k, l) <- keys ])
