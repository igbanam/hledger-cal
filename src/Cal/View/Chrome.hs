-- | Chrome shared by every view: command line, header, currency bar, KPI
-- cards and the key-hint footer. Pure.
module Cal.View.Chrome
  ( topChrome
  , card, sub, incomeCard, expensesCard, netCard, fxCard, activityCard
  , cardRow, cardRowMin, mixBlock, footerBlock
  , colourOn, barCells, thinBar, trackSegs, mixBar, mixLegend, pairedBar
  , dim, plain, spaces, segsW, signStyle, acctSegs
  , colW, acctNeed, rowValue
  ) where

import Data.List (groupBy, intersperse)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Cal.Cli (commandLine)
import Cal.Currency (chipLabel, codeOf, symbolOf)
import Cal.Doc
import Cal.Format (Fmt (..), fmtDeltaPct, fmtPct, fmtQty, fmtSigned)
import Cal.Glyphs (BoxChars (..), Glyphs (..))
import Cal.Model.Common (Activity (..), FxCause (..), Kpis (..), Mix (..))
import Cal.Model.Shared (CurrencyBar (..), DetailRow (..))
import Cal.Theme (Role (..))
import Cal.Types
import Cal.Valuation (baseQty)
import Cal.View.Types

-- * Small helpers

dim, plain :: Style
dim = st RDim
plain = st RText

spaces :: Int -> Seg
spaces n = Seg plain (T.replicate (max 0 n) " ")

-- | Income colour for a leading "+", expense colour for a leading minus.
signStyle :: Glyphs -> Text -> Style
signStyle g t
  | "+" `T.isPrefixOf` t = st RInc
  | gMinus g `T.isPrefixOf` t = st RExp
  | otherwise = plain

-- | A detail row's account (elided to @w@ columns) followed by its dim location when it fits.
acctSegs :: Glyphs -> Int -> Style -> DetailRow -> [Seg]
acctSegs g w sty r =
  let a = truncEnd g w (truncAccount w (drAccount r))
      left = w - textW a - 2
      locSegs = case drLocation r of
        Just l | left >= 3 -> [Seg dim ("  " <> truncEnd g left l)]
        _ -> []
  in Seg sty a : locSegs

-- | Widest of a column's cells (0 when empty).
colW :: (a -> Int) -> [a] -> Int
colW h = maximum . (0 :) . map h

-- | Columns an account plus its location need.
acctNeed :: DetailRow -> Int
acctNeed r = textW (drAccount r) + maybe 0 (\l -> 2 + textW l) (drLocation r)

-- | A row's valued amount, signed from the cash-flow side (expenses negative).
rowValue :: Fmt -> DetailRow -> Text
rowValue f r = fmtSigned f (if drKind r == KExpense then negate (drValue r) else drValue r)

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

-- | Word-wrapped text in one style, wrapped as 'wrapFig' wraps figures, so
-- an amount stays whole where it can and is never cut.
wrapText :: Glyphs -> Style -> Int -> Text -> Block
wrapText g s w t = linesB [ [Seg s l] | l <- wrapFig g w t ]

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
  vcat ([str dim label] ++ [ str s x | x <- wrapFig (rcGlyphs ctx) w v ] ++ rest)

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
  [sub ctx w (maybe (gDash (rcGlyphs ctx)) (fmtPct (rcFmt ctx)) (kKept k) <> " of income kept")]

fxCard :: RenderCtx -> Int -> Kpis -> Block
fxCard ctx w k = card ctx w "FX EFFECT" value $
  sub ctx w "period-end vs txn-date rates"
    : [ sub ctx w ("mostly " <> codeOf c <> " " <> grp <> " (" <> fmtQty f amt <> ")")
      | Just (FxCause c grp amt) <- [kFxCause k], kFx k /= Just 0 ]
  where
    f = rcFmt ctx
    value = case kFx k of
      Nothing -> Seg (bold dim) (gDash (rcGlyphs ctx))
      Just q -> Seg (bold (st (if q < 0 then RExp else if q > 0 then RInc else RText))) (fmtQty f q)

activityCard :: RenderCtx -> Int -> Kpis -> Block
activityCard ctx w k = card ctx w "ACTIVITY" (Seg (bold plain) (count n "txn" "txns"))
  [sub ctx w (count (length (aCommodities a)) "currency" "currencies" <> " used")]
  where
    a = kActivity k
    n = aTxns a
    count i one many = T.pack (show i) <> " " <> (if i == 1 then one else many)

-- | Cards side by side in one bordered strip, with ruled dividers and one
-- cell of padding inside every card. At 'Tier80' they wrap onto two rows
-- of the same strip.
cardRow :: RenderCtx -> [Int -> Block] -> Block
cardRow ctx = cardRowMin ctx . map ((,) 0)

-- | 'cardRow' where each card may ask for a minimum width (0: none). A card
-- whose minimum is above the even share gets it and the others share the
-- rest evenly; when they can't (the rest is below one of their minimums),
-- every card gets the even share.
cardRowMin :: RenderCtx -> [(Int, Int -> Block)] -> Block
cardRowMin _ [] = emptyB
cardRowMin ctx fs = Block w (ruleLine (bxTL bx) (bxTR bx) [] (concatMap cutsOf (take 1 built))
  : concat (zipWith between built (map Just (drop 1 built) ++ [Nothing])))
  where
    w = rcWidth ctx
    bx = gBox (rcGlyphs ctx)
    rs = st RRule
    n = length fs
    rows
      | rcTier ctx == Tier80 = let (a, b) = splitAt ((n + 1) `div` 2) fs in filter (not . null) [a, b]
      | otherwise = [fs]
    built = map row rows
    -- the columns of the dividers inside a row
    cutsOf (ws, _) = [ 3 + sum (take (i + 1) ws) + 3 * i | i <- [0 .. length ws - 2] ]
    between r next = bLines (snd r) ++
      [ case next of
          Just r' -> ruleLine (bxLT bx) (bxRT bx) (cutsOf r) (cutsOf r')
          Nothing -> ruleLine (bxBL bx) (bxBR bx) (cutsOf r) [] ]
    ruleLine l r up down = Line [Seg rs (T.pack (l : [ mid c | c <- [1 .. w - 2] ] ++ [r]))]
      where
        mid c = case (c `elem` up, c `elem` down) of
          (True, True) -> bxX bx
          (True, False) -> bxBT bx
          (False, True) -> bxTT bx
          _ -> bxH bx
    row cs =
      let k = length cs
          ws = cardWidths (w - 4 - 3 * (k - 1)) (map fst cs)
          blocks = zipWith (\(_, c) cw -> padTo cw (c cw)) cs ws
          h = maximum (map (length . bLines) blocks)
          vr = T.singleton (bxV bx)
          edge t = Block (textW t) (replicate h (Line [Seg rs t]))
          sep = edge (" " <> vr <> " ")
      in (ws, hcat 0 ([edge (vr <> " ")] ++ intersperse sep blocks ++ [edge (" " <> vr)]))

-- | Card widths summing to @total@: an even split (the first cards take the
-- odd cells), except that a card whose minimum is above the even share gets
-- its minimum and the others share the rest.
cardWidths :: Int -> [Int] -> [Int]
cardWidths total mins
  | nOther == 0 || nOther == k || any (> q) [ m | (m, False) <- zip mins big ] = spread total k
  | otherwise = go mins big (spread (total - fixed) nOther)
  where
    k = length mins
    big = [ m > total `div` k | m <- mins ]
    fixed = sum [ m | (m, True) <- zip mins big ]
    nOther = length (filter not big)
    q = (total - fixed) `div` nOther
    spread t n = let (a, r) = t `divMod` n in [ a + (if i < r then 1 else 0) | i <- [0 .. n - 1] ]
    go (m : ms) (True : bs) os = m : go ms bs os
    go (_ : ms) (False : bs) (o : os) = o : go ms bs os
    go _ _ _ = []

-- * Thin bars (spec §10.2, design S2)

-- | Is there colour? Without it, bars switch to glyphs that tell their parts apart.
colourOn :: RenderCtx -> Bool
colourOn ctx = sColor (rcSettings ctx) /= ColorNone

-- | Cells of a @w@-cell bar filled to fraction @x@ (clamped 0..1); a
-- non-zero fraction fills at least one cell.
barCells :: Int -> Double -> Int
barCells w x
  | x <= 0 || w <= 0 = 0
  | otherwise = max 1 (min w (round (min 1 x * fromIntegral w)))

-- | @k@ cells of bar track: dim @━@ in colour, @─@ without (ASCII: @-@).
trackSegs :: RenderCtx -> Int -> [Seg]
trackSegs ctx k = [ Seg (st RRule) (T.replicate k (T.singleton ch)) | k > 0 ]
  where
    g = rcGlyphs ctx
    ch = if colourOn ctx then gBarTrack g else gBarTrackPlain g

-- | A one-row bar exactly @w@ cells wide: fraction @x@ of it in @━@ (ASCII
-- @=@) in the role colour, the rest the track.
thinBar :: RenderCtx -> Role -> Int -> Double -> [Seg]
thinBar ctx role w x =
  [ Seg (st role) (T.replicate k (T.singleton (gBarThin (rcGlyphs ctx)))) | k > 0 ] ++ trackSegs ctx (max 0 w - k)
  where k = barCells w x

-- | A mix bar exactly @w@ cells wide: one segment per commodity, tinted by
-- rank (100/60/40/25 %); without colour each rank has its own glyph.
mixBar :: RenderCtx -> Kind -> Int -> Mix -> [Seg]
mixBar ctx kind w (Mix xs) = segs ++ trackSegs ctx (w - sum widths)
  where
    g = rcGlyphs ctx
    role = if kind == KIncome then RInc else RExp
    tints = [100, 60, 40, 25] ++ repeat 25
    rankCh i
      | colourOn ctx = gBarThin g
      | otherwise = case drop i (gMixRank g) of
          (c : _) -> c
          [] -> last (gBarThin g : gMixRank g)
    bounds = [min w (round (c * fromIntegral w)) | c <- scanl (+) 0 (map snd xs)] :: [Int]
    widths = zipWith (-) (drop 1 bounds) bounds
    segs = [ Seg (tinted t (st role)) (T.replicate cells (T.singleton (rankCh i)))
           | (i, t, cells) <- zip3 [0 :: Int ..] tints widths, cells > 0 ]

-- | The legend items of a mix: @symbol share@ per commodity, dim.
mixLegend :: RenderCtx -> Mix -> [[Seg]]
mixLegend ctx (Mix xs) = [ [Seg dim (fromMaybe c (symbolOf c) <> " " <> fmtPct (rcFmt ctx) sh)] | (c, sh) <- xs ]

-- | A mix bar @w@ cells wide with its legend flowed beneath it.
mixBlock :: RenderCtx -> Kind -> Int -> Mix -> Block
mixBlock ctx kind w m = vcat [line (mixBar ctx kind w m), linesB (flow w [Seg dim (gSep (rcGlyphs ctx))] (mixLegend ctx m))]

-- | An in bar over an out bar, @w@ cells wide. In UTF-8 with colour this is
-- one row of @▀@: the top half (fg) is income, the bottom half (bg) is
-- expense, and a half that doesn't extend shows the track. In ASCII or
-- without colour it is two 'thinBar' rows. 'Nothing' (native mode) gives
-- blank rows.
pairedBar :: RenderCtx -> Int -> Maybe Double -> Maybe Double -> Block
pairedBar ctx w inF outF = case gBarPair (rcGlyphs ctx) of
  Just ch | colourOn ctx -> line (cells ch)
  _ -> vcat [line (one RInc inF), line (one RExp outF)]
  where
    one role = maybe [spaces w] (thinBar ctx role w)
    ki = maybe 0 (barCells w) inF
    ko = maybe 0 (barCells w) outF
    half on role = if on then role else RRule
    styleAt i = onBg (half (i < ko) RExp) 100 (st (half (i < ki) RInc))
    cells ch
      | inF == Nothing && outF == Nothing = [spaces w]
      | otherwise = [ Seg sty (T.replicate (length grp) (T.singleton ch))
                    | grp@(sty : _) <- groupBy (==) (map styleAt [0 .. w - 1]) ]

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
