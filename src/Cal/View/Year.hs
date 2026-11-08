{-# LANGUAGE OverloadedStrings #-}
-- | The Year face: four quarter groups. At ≥ 140 columns each quarter is a
-- row of boxes, a quarter summary and three month cards (design F2);
-- narrower, a group of month rows. Pure.
module Cal.View.Year (yearDoc) where

import Data.List (intersperse)
import qualified Data.Text as T
import Data.Time.Calendar (toGregorian)

import Cal.Currency (symbolOf)
import Cal.Doc
import Cal.Format (fmtAbs, fmtDayMon, fmtPct, fmtScale, fmtSigned)
import Cal.Glyphs (BoxChars (..), Glyphs (..))
import Cal.Model.Shared (MonthRow (..))
import Cal.Model.Year
import Cal.Theme (Role (..))
import Cal.Types
import Cal.View.Chrome
import Cal.View.Types

yearOf :: Day -> Integer
yearOf d = let (y, _, _) = toGregorian d in y

yearDoc :: RenderCtx -> YearModel -> ViewDoc
yearDoc ctx m = ViewDoc
  { vdTop = top, vdBody = body, vdCursorRow = cursorRow, vdCursorLast = Nothing, vdStretch = Nothing, vdBottom = emptyB
  , vdKeys = keys, vdLegend = legend }
  where
    g = rcGlyphs ctx
    f = rcFmt ctx
    s = rcSettings ctx
    today = sToday s
    k = ymKpis m
    title = T.pack (show (yearOf (spStart (ymSpan m))))
    withAvg w t b = case splitAt 2 (bLines b) of
      (hd, tl) -> Block (bWidth b) (hd ++ bLines (sub ctx w t) ++ tl)
    avgFull = fmap (\a -> "avg " <> fmtAbs f a <> " / full month") (ymAvgExpenseFull m)
    -- at full width the EXPENSES card is wide enough for its average on one line
    expMin = if grid then maybe 0 textW avgFull else 0
    cards = cardRowMin ctx
      [ (0, \cw -> maybe id (\a -> withAvg cw ("avg " <> fmtAbs f a <> " / month")) (ymAvgIncome m) (incomeCard ctx cw "" k))
      , (expMin, \cw -> maybe id (withAvg cw) avgFull (expensesCard ctx cw "" k))
      , (0, \cw -> netCard ctx cw k)
      , (0, \cw -> fxCard ctx cw k)
      , (0, monthsCard) ]
    monthsCard cw = vcat
      [ str dim "MONTHS"
      , str (bold plain) (truncEnd g cw value)
      , overspentLine cw ]
    value
      | ymOpen m = T.pack (show (ymClosed m)) <> " + 1 open"
      | otherwise = T.pack (show (ymClosed m)) <> " closed"
    overspentLine cw = case ymOverspent m of
      [] -> str dim "none overspent"
      ds -> str (st RExp) (truncEnd g cw (T.pack (show (length ds)) <> " overspent (" <> T.intercalate ", " (map monAbbr ds) <> ")"))
    top = topChrome ctx title (ymBar m) [cards]

    allRows = [ r | q <- ymGroups m, r <- qgMonths q ]
    rowsW = rcWidth ctx
    bwFor _ = if ymScale m == Nothing then 0 else rcWidth ctx `div` 3
    rowBlocks = monthRowBlocks ctx rowsW True bwFor cursorDay allRows allRows
    grouped = go (ymGroups m) rowBlocks
      where
        go [] _ = []
        go (q : qs) rs = let (a, b) = splitAt (length (qgMonths q)) rs in (q, a) : go qs b

    grid = rcTier ctx == TierFull
    cursorDay = if rcInteractive ctx then Just (ymCursor m) else Nothing
    (body, cursorRow) = if grid then yearGrid ctx m cursorDay else (rowsBody, rowsCursor)

    -- below 140 columns: tagged blocks (is the cursor row?), a blank line between groups
    tagged = concat (intersperse [(False, blankB)]
      [ (False, header q) : rs | (q, rs) <- grouped ])
    blankB = Block 0 [Line []]
    rowsBody = vcat (map snd tagged)
    offsets = scanl (+) 0 [ length (bLines b) | (_, b) <- tagged ]
    rowsCursor
      | rcInteractive ctx = lookup True (zip (map fst tagged) offsets)
      | otherwise = Nothing

    header q = line $
      [ Seg (bold plain) ("Q" <> T.pack (show (qgIndex q))), Seg plain "  "
      , Seg (bold plain) (fmtSigned f (qgNet q)), Seg plain "  " ] ++
      case qgStatus q of
        Open -> [Seg dim ("open" <> gSep g <> "to " <> fmtDayMon today)]
        _ -> [Seg dim (maybe (gDash g) (fmtPct f) (qgSpent q) <> " spent")]

    keys = [(gKeyArrows g, "month"), (gKeyEnter g, "open month"), ("c", "currency"), ("v", "rates at"), ("/", "filter account"), ("q", "quit")]
    legend = line $
      [ Seg dim "bars: ", Seg (st RInc) (gLegendBar g), Seg dim " in ", Seg (st RExp) (gLegendBar g), Seg dim " out" ]
      ++ [ Seg dim (gSep g <> "scale " <> fmtScale f sc) | Just sc <- [ymScale m] ]
      ++ [ Seg dim (gSep g <> "symbols = currencies used") ]

-- | The ≥ 140 body (design F2): per quarter, a row of a summary box and
-- three month cards, all as tall as the tallest, with a blank row between
-- quarters; and the line of the cursor
-- card's name. The cursor card's border is the cursor (S4), the only
-- purple; otherwise an overspent month's border takes the expense tint. The
-- open month is marked by its "to 6th" chip and the "…" after its net, not
-- its border; a future month's border is dashed.
yearGrid :: RenderCtx -> YearModel -> Maybe Day -> (Block, Maybe Int)
yearGrid ctx m cursor = (vcat (intersperse (Block 0 [Line []]) rowsB), cursorRow)
  where
    g = rcGlyphs ctx
    f = rcFmt ctx
    w = rcWidth ctx
    today = sToday (rcSettings ctx)
    gap = 1
    qw = max 19 (w * 13 `div` 100)
    (cq, cr) = (w - qw - 3 * gap) `divMod` 3
    cardWs = [ cq + (if i < cr then 1 else 0) | i <- [0 .. 2 :: Int] ]
    allRows = [ r | q <- ymGroups m, r <- qgMonths q ]
    figNeed = maximum (0 : [ textW (fmtAbs f x) | r <- allRows, x <- [mrIncome r, mrExpenses r] ])
    isCursor r = case cursor of
      Just c -> spStart (mrSpan r) <= c && c < spEnd (mrSpan r)
      Nothing -> False
    rowsB = map quarterRow (ymGroups m)
    heights = map (length . bLines) rowsB
    cursorRow = case [ i | (i, q) <- zip [0 :: Int ..] (ymGroups m), any isCursor (qgMonths q) ] of
      (i : _) -> Just (sum (take i heights) + i + 1)
      [] -> Nothing

    -- content @iw@ wide in a box @n@ wide, one cell of padding each side,
    -- padded with blank lines to @h@ (@top@ of them above the content)
    inBox sty gl n h top c =
      let iw = n - 4
          ls = bLines c
          filler k = replicate k (Line [])
      in boxedIn sty gl Nothing n
           (hcat 0 [str plain " ", padTo iw (Block iw (filler top ++ ls ++ filler (h - top - length ls))), str plain " "])

    quarterRow q =
      let qc = quarterContent q
          mcs = [ (r, cw, monthContent (cw - 4) r) | (r, cw) <- zip (qgMonths q) cardWs ]
          h = maximum (length (bLines qc) : [ length (bLines c) | (_, _, c) <- mcs ])
          qBox = inBox (st RRule) g qw h ((h - length (bLines qc)) `div` 2) qc
          mBox (r, cw, c) = inBox (borderStyle r) (borderGlyphs r) cw h 0 c
      in hcat gap (qBox : map mBox mcs)

    borderStyle r
      | isCursor r = cursorBorder
      | mrOverspent r = st RExp
      | otherwise = st RRule
    ascii = sGlyphSet (rcSettings ctx) == GlyphsAscii
    -- dashed sides; the corners are dots (there are no dashed corner
    -- glyphs), so no solid corner breaks the dashed line
    dashed
      | ascii = (gBox g) { bxH = '-', bxV = ':', bxTL = '.', bxTR = '.', bxBL = '\'', bxBR = '\'' }
      | otherwise = (gBox g) { bxH = '┄', bxV = '┆', bxTL = '·', bxTR = '·', bxBL = '·', bxBR = '·' }
    borderGlyphs r
      | mrStatus r == Future = g { gBox = dashed }
      | otherwise = g

    quarterContent q =
      let iw = qw - 4
          status = case qgStatus q of
            Open -> "open" <> gSep g <> "to " <> fmtDayMon today
            _ -> maybe (gDash g) (fmtPct f) (qgSpent q) <> " spent"
      in vcat $ [str (bold plain) ("Q" <> T.pack (show (qgIndex q)))]
           ++ [ str plain t | t <- wrapFig g iw (fmtSigned f (qgNet q)) ]
           ++ [ str dim t | t <- wrapFig g iw status ]

    monthContent iw r
      | mrStatus r == Future && not (mrHasPostings r) =
          vcat $ headerLines iw r ++ [str dim "no postings yet"] ++ [ str dim t | Just h <- [mrHint r], t <- cardHint g f iw h ]
      | otherwise = vcat (headerLines iw r ++ [barRow iw RInc "in" (mrInFrac r) (mrIncome r), barRow iw RExp "out" (mrOutFrac r) (mrExpenses r)])

    -- name (bold), the open month's chip, the symbols (dim) and the net on the right
    headerLines iw r =
      let empty = mrStatus r == Future && not (mrHasPostings r)
          name = Seg (bold (if empty then dim else plain)) (monAbbr (spStart (mrSpan r)))
          chip = [ x | mrStatus r == Open, x <- [spaces 1, Seg (inverse (st RInc)) (" to " <> ordinal (dayOf today) <> " ")] ]
          left = name : chip
          netText = fmtSigned f (mrNet r)
          netSty = bold (if mrOverspent r then st RExp else plain)
          suffix = [ Seg (st RExp) (" " <> gOverspent g) | mrOverspent r ] ++ [ Seg dim (" " <> gOpen g) | mrStatus r == Open ]
          right
            | empty = [Seg dim (gDash g)]
            | otherwise = Seg netSty netText : suffix
          syms = T.unwords [ maybe c id (symbolOf c) | c <- mrCommodities r ]
          room = iw - segsW left - segsW right - 2
          symSegs
            | T.null syms || room < 1 = []
            | textW syms <= room = [Seg dim (" " <> syms)]
            | room >= textW (gOpen g) + 2 = [Seg dim (" " <> truncEnd g room syms)]
            | otherwise = []
          lw = segsW (left ++ symSegs)
      in if lw + 1 + segsW right <= iw
           then [line (left ++ symSegs ++ [spaces (iw - lw - segsW right)] ++ right)]
           else
             -- the net doesn't fit beside the name: it goes beneath, right-aligned, wrapped whole
             let sw = segsW suffix
                 parts = wrapFig g (iw - sw) netText
                 n = length parts
             in line left : [ alignR iw (line (Seg netSty t : (if i == n then suffix else [])))
                            | (i, t) <- zip [1 :: Int ..] parts ]

    -- a thin bar (scaled to the year's largest month) and the figure on the right;
    -- native (no bar), the label and the figure
    barRow iw role label frac amt =
      let fw = max 1 (min figNeed (iw - 2 - 6))
          parts = wrapFig g fw (fmtAbs f amt)
          right t = Seg plain (T.replicate (fw - textW t) " " <> t)
          native = sValuation (rcSettings ctx) == Native
      in case frac of
           -- valued with nothing to scale by (an empty year): an empty track
           Nothing | not native -> barRow iw role label (Just 0) amt
           Just x ->
             -- the bar takes about 60% of the card, as in the design; the figure sits at the right edge
             let bw = min (iw - 2 - fw) (iw * 3 `div` 5)
             in vcat [ line ((if i == 0 then thinBar ctx role bw x else [spaces bw]) ++ [spaces (iw - bw - fw), right t])
                     | (i, t) <- zip [0 :: Int ..] parts ]
           Nothing ->
             let lw = textW label + 1
                 ps = wrapFig g (iw - lw) (fmtAbs f amt)
             in vcat [ line ([Seg dim (if i == 0 then label else T.replicate (textW label) " ")] ++ [spaces (iw - textW label - textW t), Seg plain t])
                     | (i, t) <- zip [0 :: Int ..] ps ]

