{-# LANGUAGE OverloadedStrings #-}
-- | The Quarter face: a grid of quarter cards and a year-to-date strip. Pure.
module Cal.View.Quarter (quarterDoc) where

import qualified Data.Text as T
import Data.Time.Calendar (addDays, toGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime)

import Cal.Currency (codeOf)
import Cal.Doc
import Cal.Format (fmtDayMon, fmtPct, fmtQty, fmtScale, fmtSigned)
import Cal.Glyphs (Glyphs (..))
import Cal.Model.Common (Kpis (..))
import Cal.Model.Quarter
import Cal.Theme (Role (..))
import Cal.Types
import Cal.View.Chrome
import Cal.View.Types
import Cal.View.Year (monthRowBlocks)

quarterDoc :: RenderCtx -> QuarterModel -> ViewDoc
quarterDoc ctx m = ViewDoc
  { vdTop = top, vdBody = body, vdCursorRow = cursorRow, vdBottom = emptyB
  , vdKeys = keys, vdLegend = legend }
  where
    g = rcGlyphs ctx
    f = rcFmt ctx
    s = rcSettings ctx
    today = sToday s
    w = rcWidth ctx
    year = let (y, _, _) = toGregorian (spStart (qmSpan m)) in T.pack (show y)
    top = topChrome ctx (year <> " by quarter") (qmBar m) []

    -- grid
    gap = 2
    single = rcTier ctx == Tier80
    cw = if single then w else (w - gap) `div` 2
    iw = cw - 4
    perRow = if single then 1 else 2 :: Int
    chunks [] = []
    chunks xs = let (a, b) = splitAt perRow xs in a : chunks b
    allMonths = concatMap qcMonths (qmCards m)
    bwFor need = max 4 (min (iw `div` 3) (iw - 2 - 11 - 7 - need))
    monthBlocks c = map snd (monthRowBlocks ctx iw False bwFor Nothing allMonths (qcMonths c))
    contents = [ (qcIndex c, cardContent c) | c <- qmCards m ]
    gridRows = chunks contents
    rowHeights = [ maximum (map (length . bLines . snd) r) | r <- gridRows ]
    -- the cursor card's border takes the cursor style (design S4)
    boxedCard h (i, b) =
      boxedIn (if rcInteractive ctx && i == qmCursor m then cursorBorder else st RRule) g Nothing cw
        (hcat 0 [str plain " ", padTo iw (vcat [b, Block 0 (replicate (h - length (bLines b)) (Line []))])])
    gridRowBlock h r = hcat gap (map (boxedCard h) r)
    grid = [ gridRowBlock h r | (r, h) <- zip gridRows rowHeights ]

    body = vcat (grid ++ [Block 0 [Line []], strip])

    -- row of the cursor quarter's title: each box adds a border line
    cursorRow
      | not (rcInteractive ctx) = Nothing
      | otherwise = case [ i | (i, r) <- zip [0 :: Int ..] gridRows, any ((== qmCursor m) . fst) r ] of
          (i : _) -> Just (sum (take i [ h + 2 | h <- rowHeights ]) + 1)
          [] -> Nothing

    -- one card's content (without its box)
    cardContent c = vcat $ [titleLine c, figures c] ++ mixes c ++ monthBlocks c
    monAbbr d = T.pack (formatTime defaultTimeLocale "%b" d)
    enDash = if gDash g == "-" then "-" else "\8211"
    titleLine c =
      let q = Seg (bold plain) ("Q" <> T.pack (show (qcIndex c)))
          months = Seg dim ("  " <> monAbbr (spStart (qcSpan c)) <> " " <> enDash <> " " <> monAbbr (addDays (-1) (spEnd (qcSpan c))))
          left = [q, months]
          right = case qcStatus c of
            Closed -> Seg dim "closed"
            Open -> Seg (inverse (st RInc)) (" in progress" <> gSep g <> "to " <> fmtDayMon today <> " ")
            Future -> Seg dim "upcoming"
          rw = segsW [right]
          lw = segsW left
      in line (left ++ [spaces (iw - lw - rw), right])
    -- the four figure columns of a card; widths and layout are shared by all cards
    figCols wide c =
      let k = qcKpis c
          pct = maybe (gDash g) (fmtPct f) (kSpent k)
          netLabel = str dim ("net" <> gSep g <> "spent")
          net = fmtSigned f (kNet k)
          netCol
            | wide = vcat [netLabel, line [Seg (bold plain) net, Seg dim (" " <> pct)]]
            | otherwise = vcat [netLabel, str (bold plain) net, str dim pct]
      in [ vcat [str dim "income", str (bold (st RInc)) (fmtSigned f (kIncome k))]
         , vcat [str dim "expenses", str (bold (st RExp)) (fmtSigned f (negate (kExpenses k)))]
         , netCol
         , vcat [str dim "FX effect", line [maybe (Seg (bold dim) (gDash g)) fxSeg (kFx k)]] ]
    colWidths wide = foldr1 (zipWith max) [ map bWidth (figCols wide c) | c <- qmCards m ]
    wideNet = sum (colWidths True) + 2 * 3 <= iw
    colGap = if sum (colWidths wideNet) + 2 * 3 <= iw then 2 else 1
    figures c =
      let ws = colWidths wideNet
          share = max 0 (iw - sum ws - colGap * 3) `div` 4
      in hcat colGap [ padTo (cwid + share) col | (col, cwid) <- zip (figCols wideNet c) ws ]
    fxSeg q = Seg (bold (st (if q < 0 then RExp else if q > 0 then RInc else RText))) (fmtQty f q)
    mixes c
      | rcTier ctx < Tier120 = []
      | otherwise =
          [ hcat 0 [vcat [str dim "in  ", str dim "    "], mixBlock ctx KIncome (iw - 4) mx] | Just mx <- [kIncomeMix (qcKpis c)] ]
          ++ [ hcat 0 [vcat [str dim "out ", str dim "    "], mixBlock ctx KExpense (iw - 4) mx] | Just mx <- [kExpenseMix (qcKpis c)] ]


    -- year-to-date strip
    strip = boxed g Nothing w (hcat 0 [str plain " ", padTo (w - 4) (vcat stripLines)])
    kt = qmToDate m
    items =
      [ [Seg (bold plain) (year <> " to date")]
      , [Seg dim "income ", Seg (bold (st RInc)) (fmtSigned f (kIncome kt))]
      , [Seg dim "expenses ", Seg (bold (st RExp)) (fmtSigned f (negate (kExpenses kt)))]
      , [Seg dim "net ", Seg (bold plain) (fmtSigned f (kNet kt))]
      , [Seg dim "FX effect ", maybe (Seg (bold dim) (gDash g)) fxSeg (kFx kt)] ]
    sep = [spaces 2]
    stale = [ [Seg (st RWarn) (gStale g <> " " <> codeOf c <> " rate is " <> T.pack (show n) <> " days old")] | (c, n) <- qmStaleAges m ]
    flowLines [] = []
    flowLines (x0 : xs0) = go x0 xs0
      where
        go cur [] = [cur]
        go cur (y : ys)
          | segsW cur + 2 + segsW y <= w - 4 = go (cur ++ sep ++ y) ys
          | otherwise = cur : go y ys
    stripLines = case (flowLines items, stale) of
      ([l], [st1]) | segsW l + 2 + segsW st1 <= w - 4 ->
        [line (l ++ [spaces (w - 4 - segsW l - segsW st1)] ++ st1)]
      (ls, sts) -> map line ls ++ map line sts

    keys = [(gKeyArrows g, "quarter"), (gKeyEnter g, "open quarter's months"), ("c", "currency"), ("v", "rates at"), ("/", "filter account"), ("q", "quit")]
    legend = line $
      [ Seg dim "bars: ", Seg (st RInc) (gLegendBar g), Seg dim " in ", Seg (st RExp) (gLegendBar g), Seg dim " out" ]
      ++ [ Seg dim (gSep g <> "scale " <> fmtScale f sc) | Just sc <- [qmScale m] ]
