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
  { vdTop = top, vdBody = body, vdCursorRow = cursorRow
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
    rowMode = tier == Tier80
    cellW = (w - (if rowMode then 0 else 16)) `div` 7
    wnW = if rowMode then 0 else w - 7 * cellW
    -- content between a leading and a trailing space, then the rule; narrow
    -- cells drop the trailing space, the narrowest (Tier80) the leading one too
    lead = if cellW >= 12 then 1 else 0
    inner = cellW - 1 - lead - (if cellW >= 14 then 1 else 0)
    rule = st RRule
    vrule = Seg rule (T.singleton (bxV bx))
    cellLine ss = Line ([spaces lead] ++ ss ++ [spaces (cellW - 1 - lead - segsW ss), vrule])
    ruleLine end = line $
      [ Seg rule (T.replicate (cellW - 1) (T.singleton (bxH bx)) <> T.singleton (if i == 7 && rowMode then bxRT bx else end))
      | i <- [1 .. 7 :: Int] ]
      ++ [ Seg rule (T.replicate wnW (T.singleton (bxH bx))) | wnW > 0 ]
    headerLine = line $
      concat [ let Line ss = cellLine [Seg dim d] in ss | d <- ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"] ]
      ++ [ Seg dim (T.justifyRight wnW ' ' "week net") | wnW > 0 ]

    isCursor c = interactive && dcDay c == mmCursor m
    dayNum d = let (_, _, x) = toGregorian d in T.pack (show x)
    cellRows c = l1 : nets ++ natives
      where
        dayT = dayNum (dcDay c)
        daySty
          | isCursor c = inverse (bold (st RCursor))
          | dcInMonth c = bold plain
          | otherwise = st RFaded
        codeT = case dcForeign c of
          Just (cm, _, _) -> truncEnd g (max 1 (inner - textW dayT - 1)) (codeOf cm)
          Nothing -> ""
        l1 = Seg daySty dayT : [ s' | not (T.null codeT), s' <- [spaces (inner - textW dayT - textW codeT), Seg dim codeT] ]
        netT = fmtSigned f (dcNet c)
        nets = [ [Seg (signStyle g netT) x] | dcHasPostings c, x <- wrapFig g inner netT ]
        natives = case dcForeign c of
          Just (_, amt, more) | fBase f /= Nothing ->
            [ [Seg dim x] | x <- wrapFig g inner (fmtNative f amt <> (if more > 0 then " +" <> T.pack (show more) else "")) ]
          _ -> []
    weekNetCol wn = alignR wnW $ vcat $
      [ str (bold (signStyle g netT)) x | x <- wrapFig g (wnW - 1) netT ]
      ++ [ str dim x | x <- wrapFig g (wnW - 1) ("in " <> fmtAbs f (wnIn wn)) ]
      ++ [ str dim x | x <- wrapFig g (wnW - 1) ("out " <> fmtAbs f (wnOut wn)) ]
      where netT = fmtSigned f (wnNet wn)
    weekRow wn = vcat
      [ line (spaces lead : l) | l <- flowItems (w - lead) [Seg dim (gSep g)]
          [ [Seg dim "week net ", Seg (bold (signStyle g netT)) netT]
          , [Seg dim ("in " <> fmtAbs f (wnIn wn))]
          , [Seg dim ("out " <> fmtAbs f (wnOut wn))] ] ]
      where netT = fmtSigned f (wnNet wn)
    weekBlock (cells, wn) = hcat 0 (map cellBlock rowsOf ++ cols)
      where
        cols = [ weekNetCol wn | not rowMode ]
        rowsOf = map cellRows cells
        h = maximum (3 : map (length . bLines) cols ++ map length rowsOf)
        cellBlock rs = Block cellW (map cellLine (rs ++ replicate (h - length rs) []))
    weeks = mmWeeks m
    nWeeks = length weeks
    tagged =
      [(False, headerLine), (False, ruleLine (bxX bx))]
      ++ concat
        [ [ (any isCursor (fst wk), weekBlock wk)
          , (False, if rowMode then weekRow (snd wk) else ruleLine (if i == nWeeks then bxBT bx else bxX bx)) ]
        | (i, wk) <- zip [1 :: Int ..] weeks ]
    body = vcat (map snd tagged)
    offsets = scanl (+) 0 [ length (bLines b) | (_, b) <- tagged ]
    cursorRow
      | interactive = lookup True (zip (map fst tagged) offsets)
      | otherwise = Nothing

    -- * Bottom panels
    full = tier == TierFull
    ratesContent = fmap ratesLines (mmRates m)
    ratesW = maybe 0 (\b -> min (w `div` 2) (bWidth b + 4)) ratesContent
    detailW = case ratesContent of
      Just _ | full -> w - ratesW - 2
      _ -> w
    detailContent = detailLines (detailW - 4)
    pad h b = vcat [b, Block 0 (replicate (h - length (bLines b)) (Line []))]
    box title bw b = boxed g (Just (" " <> title <> " ")) bw (hcat 0 [str plain " ", padTo (bw - 4) b])
    dd = mmDetail m
    detailTitle = T.intercalate (gSep g) $
      [ fmtDayLong (ddDay dd), plural (length (ddTxns dd)) "txn" "txns", plural (ddPostings dd) "posting" "postings" ]
      ++ [ T.intercalate ", " (map codeOf cs) | let cs = ddCommodities dd, not (null cs) ]
    ratesTitle rp = "Rates" <> gSep g <> fmtDayMon (rpDate rp)
    panels = case (mmRates m, ratesContent) of
      (Just rp, Just rc)
        | full ->
            let h = max (length (bLines detailContent)) (length (bLines rc))
            in hcat 2 [box detailTitle detailW (pad h detailContent), box (ratesTitle rp) ratesW (pad h rc)]
        | otherwise -> vcat [box detailTitle detailW detailContent, blankB, box (ratesTitle rp) (min w (bWidth rc + 4)) rc]
      _ -> box detailTitle detailW detailContent

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
        room = max 0 (dI - natW - rateW - valW - 8)
        descW
          | descNeed + acctMax <= room = descNeed
          | otherwise = max 0 (min descNeed (max (room - acctMax) (room * 2 `div` 5)))
        acctW = max 0 (room - descW)
        rowLine (desc, r) =
          let sty = if drForecast r then dim else plain
              d = truncEnd g descW desc
              as = acctSegs g acctW sty r
              v = valueOf r
          in line $
            [Seg sty d, spaces (descW - textW d + 2)] ++ as ++ [spaces (acctW - segsW as + 2)]
            ++ [spaces (natW - textW (nativeOf r)), Seg sty (nativeOf r), spaces 2]
            ++ [Seg dim (rateOf r), spaces (rateW - textW (rateOf r) + 2)]
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
          | otherwise = blankB : map line (flowItems dI [Seg dim (gSep g)] [ [Seg dim p] | p <- notePieces ])

    -- rates panel: heading, then one row per commodity
    ratesLines rp = vcat (str dim heading : (if null rws then [str dim "no other commodities"] else map rateLine rws))
      where
        rws = rpRows rp
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
        source r = case (rtInfo r, rtStaleDays r) of
          (Nothing, _) -> Seg (st RWarn) "no price"
          (Just i, Just n) -> Seg (st RWarn) ("stale" <> gSep g <> fmtDayMon (riDate i) <> " (" <> T.pack (show n) <> " d)")
          (Just i, Nothing) -> Seg dim $ case riSource i of
            SrcDeclared -> "P" <> gSep g <> fmtDayMon (riDate i)
            SrcInferred -> "from txn " <> fmtDayMon (riDate i)
            SrcVia x -> "via " <> codeOf x <> gSep g <> fmtDayMon (riDate i)
        codeW = maximum (0 : map (segsW . codeSegs) rws)
        rateW = maximum (0 : map (textW . rateT) rws)
        deltaW = maximum (0 : map (textW . deltaT) rws)
        rateLine r =
          let cs = codeSegs r
          in line $
            cs ++ [spaces (codeW - segsW cs + 2)]
            ++ [spaces (rateW - textW (rateT r)), Seg plain (rateT r), spaces 2]
            ++ [spaces (deltaW - textW (deltaT r)), Seg dim (deltaT r), spaces 2, source r]

    -- * Footer
    keys = [(gKeyArrows g, "day"), ("[ ]", "month"), ("c", "currency"), ("v", "rates at"), (gKeyEnter g, "register"), ("/", "filter account"), ("q", "quit")]
    -- the cursor note only where there is a cursor
    legend = line [Seg dim ((if interactive then gCursorMark g <> " day under cursor" <> gSep g else "") <> "grey = native amount")]
