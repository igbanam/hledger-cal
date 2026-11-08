{-# LANGUAGE OverloadedStrings #-}
-- | The Week face. From 120 columns: a bordered grid of seven day columns
-- (design F5), each with its header, a spend bar, the day total and the
-- day's transactions. Below 120: seven day rows, each with its net, a spend
-- bar and the day's transactions beneath it. Pure.
module Cal.View.Week (weekDoc) where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (addDays, toGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Hledger (acommodity, amounts)

import Cal.Doc
import Cal.Format
import Cal.Glyphs (BoxChars (..), Glyphs (..), hbar, vbar)
import Cal.Model.Common (Activity (..), Kpis (..))
import Cal.Model.Shared
import Cal.Model.Week
import Cal.Theme (Role (..))
import Cal.Types
import Cal.View.Chrome
import Cal.View.Types

-- | @columnWidths avail [(lo, natural)]@: content widths summing to
-- @avail@ (see 'weekDoc'). When even the floors don't fit, an even split.
columnWidths :: Int -> [(Int, Int)] -> [Int]
columnWidths avail cols
  | sum los > avail = spread avail n
  | sum needs <= avail = topUp (map (max (level (\b -> map (max b) needs))) needs)
  | otherwise = topUp (capped (level capped))
  where
    n = length cols
    los = map fst cols
    needs = [ max lo nat | (lo, nat) <- cols ]
    capped c = [ max lo (min nat c) | (lo, nat) <- cols ]
    -- the highest level whose widths still fit
    level ws = last (0 : takeWhile (\b -> sum (ws b) <= avail) [1 .. avail])
    topUp ws = let r = avail - sum ws in zipWith (+) ws (replicate r 1 ++ repeat 0)
    spread total k = let (q, r) = total `divMod` max 1 k in replicate r (q + 1) ++ replicate (k - r) q

-- | Title: @Week 44  26 Oct – 1 Nov 2026@; both years when the week spans two.
weekTitle :: Glyphs -> WeekModel -> Text
weekTitle g m =
  "Week " <> T.pack (show (snd (wkIso m))) <> "  " <> fmtDayMon s <> yr s (fst3 s /= fst3 e) <> " " <> gDash' <> " "
    <> fmtDayMon e <> " " <> year e
  where
    s = spStart (wkSpan m)
    e = addDays (-1) (spEnd (wkSpan m))
    fst3 d = let (y, _, _) = toGregorian d in y
    year d = T.pack (show (fst3 d))
    yr d True = " " <> year d
    yr _ False = ""
    gDash' = if gDash g == "—" then "–" else "-"

weekDoc :: RenderCtx -> WeekModel -> ViewDoc
weekDoc ctx m = ViewDoc
  { vdTop = top, vdBody = body, vdCursorRow = cursorRow, vdCursorLast = cursorLast
  , vdBottom = emptyB, vdKeys = keys, vdLegend = legend }
  where
    g = rcGlyphs ctx
    f = rcFmt ctx
    w = rcWidth ctx
    interactive = rcInteractive ctx
    k = wkKpis m
    days = wkDays m
    (curDay, curTxn) = wkCursor m

    -- * Top
    count n one many = T.pack (show n) <> " " <> (if n == (1 :: Int) then one else many)
    incomeC cw
      | kIncome k == 0 = card ctx cw "INCOME" (Seg (bold (st RInc)) (fmtSigned f (kIncome k))) [sub ctx cw "none this week"]
      | otherwise = incomeCard ctx cw "" k
    expensesC cw = card ctx cw "EXPENSES" (Seg (bold (st RExp)) (fmtSigned f (negate (kExpenses k)))) $
      [ sub ctx cw (T.pack (show (round (share * 100) :: Int)) <> "% of it is " <> leaf grp) | Just (grp, share) <- [wkTop m] ]
      ++ [ mixBlock ctx KExpense cw mx | rcTier ctx >= Tier120, Just mx <- [kExpenseMix k] ]
    leaf = snd . T.breakOnEnd ":"
    activeDays = length [ () | d <- days, not (null (wdTxns d)) ]
    activityC cw = card ctx cw "ACTIVITY" (Seg (bold plain) (count (aTxns (kActivity k)) "txn" "txns"))
      (let a = T.pack (show activeDays) <> " of 7 days"
           b = count (length (aCommodities (kActivity k))) "currency" "currencies"
           whole = a <> gSep g <> b
       in if textW whole <= cw then [sub ctx cw whole] else [sub ctx cw a, sub ctx cw b])
    cards = cardRow ctx [incomeC, expensesC, \cw -> netCard ctx cw k, \cw -> fxCard ctx cw k, activityC]
    top = topChrome ctx (weekTitle g m) (wkBar m) [cards]

    -- * Body
    labelOf d = T.pack (formatTime defaultTimeLocale "%a" d) <> " " <> dayNum d
    dayNum d =
      let (_, _, x) = toGregorian d
      in T.pack (show x) <> (if x == 1 then " " <> T.pack (formatTime defaultTimeLocale "%b" d) else "")
    labelW = maximum (map (textW . labelOf . wdDay) days) + 2
    netT d = if null (wdTxns d) then "0" else fmtSigned f (wdNet d)
    netW = maximum (map (textW . netT) days)
    barW = max 4 (w `div` 4)

    hasCursor d = interactive && wdDay d == curDay
    -- all detail rows of the week, for common column widths
    allRows = [ r | d <- days, tb <- wdTxns d, r <- tbRows tb ]
    base = fBase f /= Nothing
    nativeOf r = fmtNative f (drNative r)
    rateOf r = if base then maybe "" (\q -> "@" <> fmtRate q) (drRate r) else ""
    valueOf r = if base then rowValue f r else ""
    natW = colW (textW . nativeOf) allRows
    rateW = colW (textW . rateOf) allRows
    valW = colW (textW . valueOf) allRows
    indent = 4
    acctW = max 8 (min (colW acctNeed allRows) (w - indent - natW - rateW - valW - 6))

    rowLine r =
      let as = acctSegs g acctW plain r
          v = valueOf r
      in [spaces indent] ++ as ++ [spaces (acctW - segsW as + 2)]
         ++ [spaces (natW - textW (nativeOf r)), Seg dim (nativeOf r), spaces 2]
         ++ [Seg dim (rateOf r), spaces (rateW - textW (rateOf r) + (if valW > 0 then 2 else 0))]
         ++ [spaces (valW - textW v), Seg (signStyle g v) v]
    fxLine (fx, rate) = [spaces indent, Seg dim ("FX effect " <> fmtQty f fx <> " (txn-date rate " <> fmtRate rate <> ")")]

    dayLines :: WeekDay -> [([Seg], Bool)]
    dayLines d = (head', isCur (null txnsD) ) : concat (zipWith txnLines [0 :: Int ..] txnsD)
      where
        txnsD = wdTxns d
        isCur c = c && hasCursor d
        nt = netT d
        lab = labelOf (wdDay d)
        labSty
          | isCur (null txnsD) = inverse (bold (st RCursor))
          | otherwise = bold plain
        netSeg = Seg (if null txnsD then dim else bold (signStyle g nt)) (T.justifyRight netW ' ' nt)
        barSegs = case (wdSpendFrac d, null txnsD) of
          (_, True) -> [Seg dim (gSep' g)]
          (Just x, _) -> let b = hbar g barW x in [Seg (st RExp) b]
          _ -> []
        head' = [Seg labSty lab, spaces (labelW - textW lab), netSeg, spaces 2] ++ barSegs
        txnLines i tb =
          let cur = hasCursor d && i == curTxn
              dsty = if cur then inverse (bold (st RCursor)) else bold plain
              dline = ([spaces 2, Seg dsty (truncEnd g (w - 2) (tbDescription tb))], cur)
          in dline : [ (rowLine r, False) | r <- tbRows tb ]
               ++ [ (fxLine x, False) | Just x <- [tbFx tb] ]
    gSep' gl = fromMaybe "·" (find (not . T.null) [T.strip (gSep gl)])

    tagged = concatMap dayLines days
    listBody = vcat [ line ss | (ss, _) <- tagged ]
    listCursorRow
      | interactive = lookup True (zip (map snd tagged) [0 ..])
      | otherwise = Nothing

    (body, cursorRow, cursorLast)
      | rcTier ctx >= Tier120 = (gridBody, gridCursorRow, gridCursorLast)
      | otherwise = (listBody, listCursorRow, Nothing)

    -- * Body from 120 columns: the day grid (design F5)
    -- One bordered box: a header row, a 6-row spend bar, the day total and
    -- the transactions, with a rule between the sections. A column is a
    -- divider, a space, its content and a space.
    bx = gBox g
    rule = st RRule
    vrule = Seg rule (T.singleton (bxV bx))
    barH = 6
    avail = w - 1 - 3 * 7
    -- Column widths: as even as possible, with the columns whose content
    -- needs more widened up to their natural width (descriptions, accounts
    -- and figure lines each on one line, with a 2-cell gap in a figure
    -- line; the FX line just wraps) out of what the others don't need.
    -- When that doesn't fit, the widest are capped evenly and their text
    -- wraps. No column is narrower than its widest whole figure, nor than
    -- 10 cells; a natural width counts for at most 2/7 of the room.
    colWs = columnWidths avail [ (loOf d, natOf d) | d <- days ]
    colStart i = sum [ cw + 3 | cw <- take i colWs ]
    -- the figure on the right is the valued amount, or with no valuation
    -- (native) the signed native amount; the grey native amount @rate sits
    -- on the left unless it is the base amount itself
    gridValue = rowValue f
    figLeft r
      | not base = ""
      | nativeIsBase r && drRate r == Nothing = ""
      | otherwise = T.unwords (filter (not . T.null) [nativeOf r, rateOf r])
    nativeIsBase r = all ((== fBase f) . Just . acommodity) (amounts (drNative r))
    figOneLine r = let l = figLeft r; v = gridValue r
                   in textW l + (if T.null l || T.null v then 0 else 1) + textW v
    fxText (fx, rate) = "FX effect " <> fmtQty f fx <> " (txn-date rate " <> fmtRate rate <> ")"
    dayName d = T.pack (formatTime defaultTimeLocale "%a" d)
    headW d = textW (dayName (wdDay d)) + 1 + textW (dayNum (wdDay d))
    natOf d = min ((2 * avail) `div` 7) $ maximum $
      headW d : textW (netT d)
      : concat [ textW (tbDescription tb)
                 : concat [ [textW (drAccount r), maybe 0 textW (drLocation r), figOneLine r + 1] | r <- tbRows tb ]
               | tb <- wdTxns d ]
    loOf d = maximum $ 10 : headW d : textW (netT d)
      : [ textW x | tb <- wdTxns d, r <- tbRows tb, x <- [gridValue r, nativeOf r, rateOf r] ]

    -- the month most of the week lies in (Thursday's, as for ISO weeks);
    -- days in another month have a dim header
    weekMonth = let (_, mo, _) = toGregorian (addDays 3 (spStart (wkSpan m))) in mo
    inWeekMonth d = let (_, mo, _) = toGregorian d in mo == weekMonth
    headerSegs cw d =
      let nm = dayName (wdDay d)
          num = dayNum (wdDay d)
          (nSty, dSty) = if inWeekMonth (wdDay d) then (bold plain, plain) else (dim, dim)
      in [Seg nSty nm, spaces (cw - textW nm - textW num), Seg dSty num]
    barRows cw d = case wdSpendFrac d of
      Just x | not (null (wdTxns d)) -> [ if c == ' ' then [] else [Seg (st RExp) (T.replicate cw (T.singleton c))] | c <- vbar g barH x ]
      _ -> replicate barH []
    totalRows cw d =
      let t = netT d
          sty = if null (wdTxns d) then dim else bold (signStyle g t)
      in [ [spaces (cw - textW x), Seg sty x] | x <- wrapFig g cw t ]
    -- an account broken after its colons, so its parts stay whole
    acctLines cw a = fill (concatMap chunk (parts a))
      where
        parts t = case T.breakOn ":" t of
          (x, y) | T.null y -> [x]
                 | otherwise -> (x <> ":") : parts (T.drop 1 y)
        chunk p = if textW p > cw then T.chunksOf cw p else [p]
        fill [] = []
        fill (x : xs) = go x xs
          where
            go cur [] = [cur]
            go cur (y : ys)
              | textW cur + textW y <= cw = go (cur <> y) ys
              | otherwise = cur : go y ys
    -- (lines, the selected transaction's first and last line)
    txnRows cw d
      | null (wdTxns d) = ([[Seg dim (gSep' g)]], Nothing)
      | otherwise = (concat blocks, sel)
      where
        blocks = zipWith txnBlock [0 :: Int ..] (wdTxns d)
        starts = scanl (+) 0 (map length blocks)
        sel
          | hasCursor d = case drop curTxn (zip starts blocks) of
              ((s0, b) : _) -> Just (s0, s0 + length b - 1)
              [] -> Nothing
          | otherwise = Nothing
        txnBlock i tb =
          let selected = hasCursor d && i == curTxn
              forecast = all drForecast (tbRows tb)
              dsty
                | selected = inverse (bold (st RCursor))
                | forecast = dim
                | otherwise = bold plain
              descL x = if selected then [Seg dsty (x <> T.replicate (cw - textW x) " ")] else [Seg dsty x]
          in map descL (wrapFig g cw (tbDescription tb))
             ++ concatMap (postingRows cw) (tbRows tb)
             ++ [ [Seg dim x] | Just fx <- [tbFx tb], x <- wrapFig g cw (fxText fx) ]
             ++ [[Seg rule (T.replicate cw (T.singleton (gDashRule g)))]]
    postingRows cw r =
      [ [Seg dim x] | x <- acctLines cw (drAccount r) ]
      ++ [ [Seg dim x] | Just loc <- [drLocation r], x <- wrapFig g cw loc ]
      ++ figRows
      where
        l = figLeft r
        v = gridValue r
        lsty = dim
        vSeg x = [spaces (cw - textW x), Seg (signStyle g x) x]
        figRows
          | T.null l && T.null v = []
          | T.null v = [ [Seg lsty x] | x <- wrapFig g cw l ]
          | T.null l = map vSeg (wrapFig g cw v)
          | figOneLine r <= cw = [[Seg lsty l, spaces (cw - textW l - textW v), Seg (signStyle g v) v]]
          | otherwise = [ [Seg lsty x] | x <- wrapFig g cw l ] ++ map vSeg (wrapFig g cw v)
    section rowsOf =
      let cols = zipWith rowsOf colWs days
          h = maximum (1 : map length cols)
          cell cw c j = case drop j c of
            (ss : _) -> ss ++ [spaces (cw - segsW ss)]
            [] -> [spaces cw]
      in [ line (vrule : concat [ [spaces 1] ++ cell cw c j ++ [spaces 1, vrule] | (cw, c) <- zip colWs cols ])
         | j <- [0 .. h - 1] ]
    ruleLine l mid r = line
      [ Seg rule (T.singleton l <> T.intercalate (T.singleton mid)
          [ T.replicate (cw + 2) (T.singleton (bxH bx)) | cw <- colWs ] <> T.singleton r) ]
    txnCols = zipWith txnRows colWs days
    headerLs = section headerSegs'
      where headerSegs' cw d = [headerSegs cw d]
    barLs = section barRows
    totalLs = section totalRows
    txnLs = section (\cw d -> fst (txnRows cw d))
    midRule = ruleLine (bxLT bx) (bxX bx) (bxRT bx)
    gridLines = [ruleLine (bxTL bx) (bxTT bx) (bxTR bx)] ++ headerLs ++ [midRule] ++ barLs ++ [midRule]
      ++ totalLs ++ [midRule] ++ txnLs ++ [ruleLine (bxBL bx) (bxBT bx) (bxBR bx)]
    txnTop = 1 + length headerLs + 1 + length barLs + 1 + length totalLs + 1
    curCol = lookup True (zip (map hasCursor days) [0 :: Int ..])
    curSel = case curCol of
      Just i -> snd (txnCols !! i)
      Nothing -> Nothing
    gridCursorRow
      | not interactive = Nothing
      | otherwise = Just (maybe 1 ((+ txnTop) . fst) curSel)
    gridCursorLast = (+ txnTop) . snd <$> curSel
    -- the cursor day's column: its share of the box's lines (design S4)
    gridBody =
      let b = vcat gridLines
      in case curCol of
           Just i -> cursorRect (0, colStart i) (length gridLines - 1, colStart (i + 1)) b
           Nothing -> b

    -- * Footer
    keys = [(gKeyLeftRight g, "day"), (gKeyUpDown g, "transaction"), ("[ ]", "week"), ("c", "currency"), ("v", "rates at"), ("/", "filter account"), ("q", "quit")]
    legend = line [Seg dim ("bar = day's spend" <> gSep g <> "grey = native amount @ rate")]
