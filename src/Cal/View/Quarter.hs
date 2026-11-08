{-# LANGUAGE OverloadedStrings #-}
-- | The Quarter face: a grid of quarter cards and a year-to-date strip. Pure.
module Cal.View.Quarter (quarterDoc) where

import Data.List (intercalate)
import qualified Data.Text as T
import Data.Time.Calendar (addDays, toGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime)

import Cal.Currency (codeOf)
import Cal.Doc
import Cal.Format (fmtDayMon, fmtPct, fmtQty, fmtScale, fmtSigned)
import Cal.Glyphs (Glyphs (..))
import Cal.Model.Common (Kpis (..), Mix)
import Cal.Model.Shared (MonthRow (..))
import Cal.Model.Quarter
import Cal.Theme (Role (..))
import Cal.Types
import Cal.View.Chrome
import Cal.View.Types
import Cal.View.Year (cardHint, monthRowBlocks)

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
    blank = Block 0 [Line []]
    bwFor need = max 4 (min (iw `div` 3) (iw - 2 - 11 - 7 - need))
    monthBlocks c = map snd (monthRowBlocks ctx iw False bwFor Nothing allMonths (qcMonths c))
    contents = [ (qcIndex c, cardContent c) | c <- qmCards m ]
    gridRows = chunks contents
    -- a card is its upper part, flexible space, and its month rows at the bottom
    cardH (up, low) = length (bLines up) + length (bLines low)
    rowHeights = [ maximum (map (cardH . snd) r) | r <- gridRows ]
    -- the cursor card's border takes the cursor style (design S4); the
    -- current quarter is marked by its chip alone (purple is the cursor only)
    boxedCard h (i, (up, low)) =
      boxedIn (if rcInteractive ctx && i == qmCursor m then cursorBorder else st RRule) g Nothing cw
        (hcat 0 [str plain " ", padTo iw (vcat [up, Block 0 (replicate (h - cardH (up, low)) (Line [])), low])])
    gridRowBlock h r = hcat gap (map (boxedCard h) r)
    grid = [ gridRowBlock h r | (r, h) <- zip gridRows rowHeights ]

    body = vcat (grid ++ [Block 0 [Line []], strip])

    -- row of the cursor quarter's title: each box adds a border line
    cursorRow
      | not (rcInteractive ctx) = Nothing
      | otherwise = case [ i | (i, r) <- zip [0 :: Int ..] gridRows, any ((== qmCursor m) . fst) r ] of
          (i : _) -> Just (sum (take i [ h + 2 | h <- rowHeights ]) + 1)
          [] -> Nothing

    -- one card's content (without its box): the part above the flexible
    -- space and the month rows below it. Two columns (≥ 100) follow the
    -- design's rhythm (F3): header, blank, labels and figures, blank, the in
    -- mix bar with its legend beside it, blank, the out one, then the month
    -- rows as paired bars. One column (80–99) keeps the compact §10.5 card.
    cardContent c
      | single = (vcat ([titleLine c, figures c] ++ monthBlocks c), emptyB)
      | otherwise = (vcat ([titleLine c, blank, figures c] ++ mixRows c ++ [blank]), vcat (map monthRow (qcMonths c)))
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

    -- the label column shared by the mix and month rows
    lead = 5
    mixesOf :: QuarterCard -> [(Kind, T.Text, Mix)]
    mixesOf c = [ (KIncome, "in", mx) | Just mx <- [kIncomeMix (qcKpis c)] ] ++ [ (KExpense, "out", mx) | Just mx <- [kExpenseMix (qcKpis c)] ]
    legendSep = [Seg dim (gSep g)]
    legendW mx = segsW (intercalate legendSep (mixLegend ctx mx))
    maxLegend = maximum (0 : [ legendW mx | c <- qmCards m, (_, _, mx) <- mixesOf c ])
    -- about 40% of the card, less where the widest legend needs the room
    mixBw = max 8 (min (iw * 2 `div` 5) (iw - lead - 2 - maxLegend))
    mixRows c
      | rcTier ctx < Tier120 = []
      | otherwise = concat [ [blank, mixRow kind label mx] | (kind, label, mx) <- mixesOf c ]
    mixRow kind label mx =
      let room = iw - lead - mixBw - 2
          legendLines = case flow room legendSep (mixLegend ctx mx) of
            [] -> [[]]
            ls -> ls
      in vcat [ line ((if i == 0 then Seg dim label : [spaces (lead - textW label)] ++ mixBar ctx kind mixBw mx
                       else [spaces (lead + mixBw)]) ++ [spaces 2] ++ l)
              | (i, l) <- zip [0 :: Int ..] legendLines ]

    -- month rows: the name, one paired in/out bar, the net at the card's right edge
    allMonthRows = concatMap qcMonths (qmCards m)
    netSuffix r = [ Seg (st RExp) (" " <> gOverspent g) | mrOverspent r ] ++ [ Seg dim (" " <> gOpen g) | mrStatus r == Open ]
    isEmpty r = mrStatus r == Future && not (mrHasPostings r)
    rightNeed = maximum (0 : [ textW (fmtSigned f (mrNet r)) + segsW (netSuffix r) | r <- allMonthRows, not (isEmpty r) ])
    rightW = min rightNeed (iw - lead - 4 - 3)
    monthBw = max 4 (min (iw * 7 `div` 10) (iw - lead - 3 - rightW))
    monthName r = Seg plain (monAbbr (spStart (mrSpan r)))
    monthRow r
      | isEmpty r = emptyMonthRow r
      | otherwise =
          let suffix = netSuffix r
              sty = bold (if mrOverspent r then st RExp else plain)
              -- native: no bars, so the net has the row to itself
              bw = if mrInFrac r == Nothing && mrOutFrac r == Nothing then 0 else monthBw
              parts = wrapFig g (max 1 (iw - lead - bw - 1 - segsW suffix)) (fmtSigned f (mrNet r))
              n = length parts
              net = vcat [ line (Seg sty t : (if i == n then suffix else [])) | (i, t) <- zip [1 :: Int ..] parts ]
          in hcat 0 $ [padTo lead (line [monthName r])] ++ [ pairedBar ctx bw (mrInFrac r) (mrOutFrac r) | bw > 0 ]
                      ++ [alignR (iw - lead - bw) net]
    -- a future month: "· no postings yet (forecast …)", the hint beneath
    -- when it doesn't fit beside it
    emptyMonthRow r =
      let noPost = T.strip (gSep g) <> " no postings yet"
          nameSegs = [monthName r, spaces (lead - textW (monAbbr (spStart (mrSpan r))))]
          first = nameSegs ++ [Seg dim noPost]
          beside = iw - lead - textW noPost - 1 - 2
          paren ls = case ls of
            [] -> []
            [x] -> ["(" <> x <> ")"]
            (x : xs) -> ("(" <> x) : init xs ++ [last xs <> ")"]
      in case mrHint r of
           Nothing -> line first
           Just h -> case cardHint g f beside h of
             [x] | beside > 0 -> line (first ++ [Seg dim (" (" <> x <> ")")])
             _ -> vcat (line first : [ line [spaces lead, Seg dim t] | t <- paren (cardHint g f (iw - lead - 2) h) ])

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
