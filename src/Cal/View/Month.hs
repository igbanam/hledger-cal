-- | The Month face: the calendar grid with week nets, then the day detail
-- and the rates panel. Pure.
module Cal.View.Month (monthDoc) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (addDays, toGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Hledger (acommodity, amounts)

import Cal.Currency (codeOf, symbolOf)
import Cal.Doc
import Cal.Format
import Cal.Glyphs (BoxChars (..), Glyphs (..))
import Cal.Model.Common (Money)
import Cal.Model.Month
import Cal.Model.Shared
import Cal.Prices (RateInfo (..), RateSource (..))
import Cal.Theme (Role (..))
import Cal.Types
import Cal.View.Chrome
import Cal.View.Types

blankB :: Block
blankB = Block 0 [Line []]

-- | Greedy flow of items into lines of at most @w@ columns, @sep@ between them.
flowItems :: Int -> [Seg] -> [[Seg]] -> [[Seg]]
flowItems _ _ [] = []
flowItems w sep (x0 : xs0) = go x0 xs0
  where
    go cur [] = [cur]
    go cur (y : ys)
      | segsW cur + segsW sep + segsW y <= w = go (cur ++ sep ++ y) ys
      | otherwise = cur : go y ys

plural :: Int -> Text -> Text -> Text
plural n one many = T.pack (show n) <> " " <> (if n == 1 then one else many)

monthDoc :: RenderCtx -> MonthModel -> ViewDoc
monthDoc ctx m = ViewDoc
  { vdTop = top, vdBody = body, vdCursorRow = cursorRow, vdCursorLast = Nothing, vdStretch = Nothing
  , vdBottom = panels
  , vdKeys = keys, vdLegend = legend }
  where
    g = rcGlyphs ctx
    f = rcFmt ctx
    s = rcSettings ctx
    w = rcWidth ctx
    tier = rcTier ctx
    bx = gBox g
    start = spStart (mmSpan m)
    k = mmKpis m
    interactive = rcInteractive ctx
    baseSym = case fBase f of
      Just b -> fromMaybe b (symbolOf b)
      Nothing -> ""

    -- * Top
    prevLabel = T.pack (formatTime defaultTimeLocale "%b" (addDays (-1) start))
    cards = cardRow ctx
      [ \cw -> incomeCard ctx cw prevLabel k
      , \cw -> expensesCard ctx cw prevLabel k
      , \cw -> netCard ctx cw k
      , \cw -> fxCard ctx cw k
      , \cw -> activityCard ctx cw k ]
    top = topChrome ctx (fmtMonthYear start) (mmBar m) [cards]

    -- * Grid
    -- One bordered box (design F4): a left edge, seven day columns that each
    -- end in a divider, then the week-net column and the right edge. At
    -- full width the eight columns are as even as the week nets allow; at
    -- 100–139 the day columns share what the week nets leave; at 80–99 the
    -- week net is a row of its own under each week.
    rowMode = tier == Tier80
    weeks = mmWeeks m
    nWeeks = length weeks
    wnTexts wn = [fmtSigned f (wnNet wn), "in " <> fmtAbs f (wnIn wn), "out " <> fmtAbs f (wnOut wn)]
    wnNeed = maximum (8 : [ textW x | (_, wn) <- weeks, x <- wnTexts wn ]) + 2
    spread total n = let (q, r) = total `divMod` n in replicate r (q + 1) ++ replicate (n - r) q
    (colWs, wnW)
      | rowMode = (spread (w - 1) 7, 0)
      | tier == TierFull =
          let wnCol = max ((w - 1) `div` 8) (wnNeed + 1)
          in (spread (w - 1 - wnCol) 7, wnCol - 1)
      | otherwise = let cw = (w - 2 - 14) `div` 7 in (replicate 7 cw, w - 2 - 7 * cw)
    minCol = minimum colWs
    -- content between a leading and a trailing space, then the divider;
    -- narrow cells drop the trailing space, the narrowest the leading one too
    -- at 80–99 a leading space too when every figure still fits beside it
    lead = if minCol >= 12 || cellNeed <= minCol - 2 then 1 else 0
    cellNeed = maximum (0 : [ textW t | wk <- weeks, c <- fst wk, t <- cellFigures c ])
    cellFigures c = [ fmtSigned f (dcNet c) | dcHasPostings c ] ++ maybe [] pure (nativeText c)
    nativeText c = case dcForeign c of
      Just (_, amt, more) | fBase f /= Nothing -> Just (fmtNative f amt <> (if more > 0 then " +" <> T.pack (show more) else ""))
      _ -> Nothing
    trail = if minCol >= 14 then 1 else 0
    innerOf cw = cw - 1 - lead - trail
    rule = st RRule
    vrule = Seg rule (T.singleton (bxV bx))
    hrule n = T.replicate n (T.singleton (bxH bx))
    -- the left edge of day column i, counted from the box's left edge
    colStart i = sum (take i colWs)
    cellSegs cw ss = [spaces lead] ++ ss ++ [spaces (cw - 1 - lead - segsW ss), vrule]
    -- the week-net column: the panel tint under every cell, padding included
    wnTint = onBg RPanel 100
    wnSegs sty t = [Seg (wnTint plain) (T.replicate (wnW - 1 - textW t) " "), Seg (wnTint sty) t, Seg (wnTint plain) " "]
    gridLine cells wn = line ([vrule] ++ concat (zipWith cellSegs colWs cells) ++ (if wnW > 0 then wn ++ [vrule] else []))
    -- a horizontal rule: left end, joints under the dividers, right end
    ruleLine l mid r = line
      [ Seg rule (T.singleton l <> T.concat
          [ hrule (cw - 1) <> T.singleton (if i == 7 && wnW == 0 then r else mid) | (i, cw) <- zip [1 :: Int ..] colWs ]
          <> (if wnW > 0 then hrule wnW <> T.singleton r else "")) ]
    headerLine = gridLine
      [ [Seg dim d] | d <- ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"] ]
      (wnSegs dim "week net")

    isCursor c = interactive && dcDay c == mmCursor m
    dayNum d = let (_, _, x) = toGregorian d in T.pack (show x)
    cellRows cw c = l1 : nets ++ natives
      where
        inner = innerOf cw
        dayT = dayNum (dcDay c)
        daySty
          | dcInMonth c = bold plain
          | otherwise = st RFaded
        codeT = case dcForeign c of
          Just (cm, _, _) -> truncEnd g (max 1 (inner - textW dayT - 1)) (codeOf cm)
          Nothing -> ""
        l1 = Seg daySty dayT : [ s' | not (T.null codeT), s' <- [spaces (inner - textW dayT - textW codeT), Seg dim codeT] ]
        netT = fmtSigned f (dcNet c)
        nets = [ [Seg (signStyle g netT) x] | dcHasPostings c, x <- wrapFig g inner netT ]
        natives = [ [Seg dim x] | Just t <- [nativeText c], x <- wrapFig g inner t ]
    weekNetLines wn =
      [ (bold (signStyle g netT), x) | x <- wrapFig g (wnW - 1) netT ]
      ++ [ (dim, x) | t <- drop 1 (wnTexts wn), x <- wrapFig g (wnW - 1) t ]
      where netT = fmtSigned f (wnNet wn)
    -- Tier80: the week net as a line of its own, inside the box
    weekRow wn = vcat
      [ line ([vrule, spaces 1] ++ l ++ [spaces (w - 3 - segsW l), vrule])
      | l <- flowItems (w - 3) [Seg dim (gSep g)]
          [ [Seg dim "week net ", Seg (bold (signStyle g netT)) netT]
          , [Seg dim ("in " <> fmtAbs f (wnIn wn))]
          , [Seg dim ("out " <> fmtAbs f (wnOut wn))] ] ]
      where netT = fmtSigned f (wnNet wn)
    weekBlock (cells, wn) = vcat
      [ gridLine [ at j rs | rs <- rowsOf ] (if wnW > 0 then maybe (wnSegs plain "") (uncurry wnSegs) (atM j wnLs) else [])
      | j <- [0 .. h - 1] ]
      where
        rowsOf = zipWith cellRows colWs cells
        wnLs = if wnW > 0 then weekNetLines wn else []
        h = maximum (3 : length wnLs : map length rowsOf)
        at j rs = fromMaybe [] (atM j rs)
        atM j xs = case drop j xs of
          (x : _) -> Just x
          [] -> Nothing
    tagged =
      [(False, ruleLine (bxTL bx) (bxTT bx) (bxTR bx)), (False, headerLine), (False, ruleLine (bxLT bx) (bxX bx) (bxRT bx))]
      ++ concat
        [ (any isCursor (fst wk), weekBlock wk) : after i wk
        | (i, wk) <- zip [1 :: Int ..] weeks ]
    after i wk
      | rowMode =
          [ (False, ruleLine (bxLT bx) (bxBT bx) (bxRT bx)), (False, weekRow (snd wk))
          , (False, if i == nWeeks then ruleLine (bxBL bx) (bxH bx) (bxBR bx) else ruleLine (bxLT bx) (bxTT bx) (bxRT bx)) ]
      | otherwise =
          [ (False, if i == nWeeks then ruleLine (bxBL bx) (bxBT bx) (bxBR bx) else ruleLine (bxLT bx) (bxX bx) (bxRT bx)) ]
    offsets = scanl (+) 0 [ length (bLines b) | (_, b) <- tagged ]
    cursorRow
      | interactive = lookup True (zip (map fst tagged) offsets)
      | otherwise = Nothing
    -- the cursor day's cell: its share of the rules around it (design S4)
    cursorCol = case [ i | wk <- weeks, (i, c) <- zip [0 :: Int ..] (fst wk), isCursor c ] of
      (i : _) -> Just i
      [] -> Nothing
    withCursor b = case (cursorRow, cursorCol) of
      (Just r, Just i) ->
        let h = maybe 0 (length . bLines) (lookup r (zip offsets (map snd tagged)))
        in cursorRect (r - 1, colStart i) (r + h, colStart (i + 1)) b
      _ -> b
    body = withCursor (vcat (map snd tagged))

    -- * Bottom panels
    -- Plain boxes that open with a title row (design F4): the subject in
    -- bold on the left, its summary dimmed on the right.
    full = tier == TierFull
    ratesNatural = fmap (\rp -> ratesLines rp 0) (mmRates m)
    -- at full width the rates panel takes about the artboard's share
    ratesW = maybe 0 (\b -> min (w `div` 2) (max (bWidth b + 4) (if full then w * 9 `div` 25 else 0))) ratesNatural
    ratesContent = fmap (\rp -> ratesLines rp (ratesW - 4)) (mmRates m)
    detailW = case ratesContent of
      Just _ | full -> w - ratesW - 2
      _ -> w
    detailContent = vcat [titleRow (detailW - 4) detailLeft detailRight, detailLines (detailW - 4)]
    pad h b = vcat [b, Block 0 (replicate (h - length (bLines b)) (Line []))]
    box bw b = boxed g Nothing bw (hcat 0 [str plain " ", padTo (bw - 4) b])
    -- left and right on one row when they fit, else the right part under it
    titleRow tw l r
      | textW l + 2 + textW r <= tw = line [Seg (bold plain) l, spaces (tw - textW l - textW r), Seg dim r]
      | otherwise = vcat [str (bold plain) (truncEnd g tw l), line [spaces (tw - textW r'), Seg dim r']]
      where r' = truncEnd g tw r
    dd = mmDetail m
    detailLeft = fmtDayLong (ddDay dd)
    detailRight = T.intercalate (gSep g) $
      [ plural (length (ddTxns dd)) "txn" "txns", plural (ddPostings dd) "posting" "postings" ]
      ++ [ T.intercalate ", " (map codeOf cs) | let cs = ddCommodities dd, not (null cs) ]
    panels = case ratesContent of
      Just rc
        | full ->
            let h = max (length (bLines detailContent)) (length (bLines rc))
            in hcat 2 [box detailW (pad h detailContent), box ratesW (pad h rc)]
        | otherwise -> vcat [box detailW detailContent, blankB, box (min w (bWidth rc + 4)) rc]
      _ -> box detailW detailContent

    -- day detail: one row per posting, then the note
    detailLines dI
      | null (ddTxns dd) = str dim "no postings"
      | otherwise = vcat (map rowLine rows ++ noteLines)
      where
        rows = [ (if i == 0 then tbDescription tb else "", r) | tb <- ddTxns dd, (i, r) <- zip [0 :: Int ..] (tbRows tb) ]
        nativeOf r = fmtNative f (drNative r)
        nativeIsBase r = all ((== fBase f) . Just . acommodity) (amounts (drNative r))
        rateOf r = case drRate r of
          Just q -> "@ " <> baseSym <> fmtRate q
          Nothing | fBase f == Nothing || nativeIsBase r -> "native"
                  | otherwise -> gDash g
        valueOf = rowValue f
        natW = colW (textW . nativeOf . snd) rows
        rateW = colW (textW . rateOf . snd) rows
        valW = colW (textW . valueOf . snd) rows
        descNeed = colW (textW . fst) rows
        acctMax = colW (acctNeed . snd) rows
        -- the native amount ends at about 71% of the panel, as in the
        -- artboard, with the rate after it and the valued figure at the
        -- right edge; it moves right for long descriptions and accounts
        natEndMax = dI - rateW - valW - 4
        natEnd = min natEndMax (max (dI * 71 `div` 100) (descNeed + acctMax + 4 + natW))
        room = max 0 (natEnd - natW - 4)
        valGap = max 2 (dI - natEnd - 2 - rateW - valW)
        descW
          | descNeed + acctMax <= room = descNeed
          | otherwise = max 0 (min descNeed (max (room - acctMax) (room * 2 `div` 5)))
        acctW = max 0 (room - descW)
        rowLine (desc, r) =
          let sty = if drForecast r then dim else plain
              d = truncEnd g descW desc
              dsty = if drForecast r then dim else bold plain
              as = acctSegs g acctW sty r
              v = valueOf r
          in line $
            [Seg dsty d, spaces (descW - textW d + 2)] ++ as ++ [spaces (acctW - segsW as + 2)]
            ++ [spaces (natW - textW (nativeOf r)), Seg sty (nativeOf r), spaces 2]
            ++ [Seg dim (rateOf r), spaces (rateW - textW (rateOf r) + valGap)]
            ++ [spaces (valW - textW v), Seg (signStyle g v) v]
        foreignKinds =
          [ drKind r | (_, r) <- rows, any ((/= fBase f) . Just . acommodity) (amounts (drNative r)) ]
        signed v = if not (null foreignKinds) && all (== KExpense) foreignKinds then negate v else v
        notePieces = case ddNote dd of
          NoteFx r v x ->
            [ "at the txn-date rate (" <> baseSym <> fmtRate r <> ") this was " <> fmtSigned f (signed (v :: Money))
            , "FX effect " <> fmtQty f x ]
          NoteNoConversion c -> ["already in " <> codeOf c, "no conversion", "no FX effect"]
          NoteNone -> []
        noteLines
          | null notePieces = []
          | otherwise = map line (flowItems dI [Seg dim (gSep g)] [ [Seg dim p] | p <- notePieces ])

    -- rates panel: the title row, then one row per commodity, the fresh
    -- rates first and the stale or missing ones after them (as in the
    -- artboard). @rw@ is the width to fill: space beyond the natural width
    -- goes between the code and the rate, so Δ stays by its source.
    ratesLines rp rw = vcat (titleRow tw title heading
                              : (if null rws then [str dim "no other commodities"] else map rateLine rws))
      where
        title = "Rates" <> gSep g <> fmtDayMon (rpDate rp)
        rws = filter (not . unsure) (rpRows rp) ++ filter unsure (rpRows rp)
        unsure r = isStale r || rtInfo r == Nothing
        bsym = fromMaybe (rpBase rp) (symbolOf (rpBase rp))
        delta = gDelta g
        heading
          | rpBase rp == sDefaultBase s = bsym <> " per 1 unit" <> gSep g <> delta <> " month"
          | otherwise = "per " <> bsym <> "1" <> gSep g <> delta <> " month"
        isStale r = rtStaleDays r /= Nothing
        codeSegs r = Seg plain (codeOf (rtCommodity r)) : [ Seg (st RWarn) (" " <> gStale g) | isStale r ]
        rateT r = maybe (gDash g) (fmtRate . riRate) (rtInfo r)
        deltaT r = case rtDeltaPct r of
          Just d | not (isStale r) -> fmtDeltaPct f d
          _ -> gDash g
        sourceT r = case (rtInfo r, rtStaleDays r) of
          (Nothing, _) -> "no price"
          (Just i, Just n) -> "stale" <> gSep g <> fmtDayMon (riDate i) <> " (" <> T.pack (show n) <> " d)"
          (Just i, Nothing) -> case riSource i of
            SrcDeclared -> "P" <> gSep g <> fmtDayMon (riDate i)
            SrcInferred -> "from txn " <> fmtDayMon (riDate i)
            SrcVia x -> "via " <> codeOf x <> gSep g <> fmtDayMon (riDate i)
        source r = Seg (if unsure r then st RWarn else dim) (sourceT r)
        codeW = maximum (0 : map (segsW . codeSegs) rws)
        rateW = maximum (0 : map (textW . rateT) rws)
        deltaW = maximum (0 : map (textW . deltaT) rws)
        srcW = maximum (0 : map (textW . sourceT) rws)
        natural = codeW + 2 + rateW + 2 + deltaW + 2 + srcW
        tw = max rw (max natural (textW title + 2 + textW heading))
        extra = max 0 (rw - natural)
        gap1 = 2 + extra
        gap2 = 2 :: Int
        rateLine r =
          let cs = codeSegs r
          in line $
            cs ++ [spaces (codeW - segsW cs + gap1)]
            ++ [spaces (rateW - textW (rateT r)), Seg plain (rateT r), spaces gap2]
            ++ [spaces (deltaW - textW (deltaT r)), Seg dim (deltaT r), spaces 2, source r]

    -- * Footer
    keys = [(gKeyArrows g, "day"), ("[ ]", "month"), ("c", "currency"), ("v", "rates at"), (gKeyEnter g, "register"), ("/", "filter account"), ("q", "quit")]
    -- the cursor note only where there is a cursor
    legend = line [Seg dim ((if interactive then gCursorMark g <> " day under cursor" <> gSep g else "") <> "grey = native amount")]
