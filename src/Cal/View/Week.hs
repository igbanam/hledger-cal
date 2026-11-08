{-# LANGUAGE OverloadedStrings #-}
-- | The Week face: seven day rows, each with its net, a spend bar and the
-- day's transactions beneath it. Pure.
module Cal.View.Week (weekDoc) where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (addDays, toGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime)

import Cal.Doc
import Cal.Format
import Cal.Glyphs (Glyphs (..), hbar)
import Cal.Model.Common (Activity (..), Kpis (..))
import Cal.Model.Shared
import Cal.Model.Week
import Cal.Theme (Role (..))
import Cal.Types
import Cal.View.Chrome
import Cal.View.Types

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
  { vdTop = top, vdBody = body, vdCursorRow = cursorRow
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
    body = vcat [ line ss | (ss, _) <- tagged ]
    cursorRow
      | interactive = lookup True (zip (map snd tagged) [0 ..])
      | otherwise = Nothing

    -- * Footer
    keys = [(gKeyLeftRight g, "day"), (gKeyUpDown g, "transaction"), ("[ ]", "week"), ("c", "currency"), ("v", "rates at"), ("/", "filter account"), ("q", "quit")]
    legend = line [Seg dim ("bar = day's spend" <> gSep g <> "grey = native amount @ rate")]
