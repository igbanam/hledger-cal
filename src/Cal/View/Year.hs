{-# LANGUAGE OverloadedStrings #-}
-- | The Year face: four quarter groups of month rows. Pure.
module Cal.View.Year (yearDoc) where

import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (toGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime)

import Cal.Currency (symbolOf)
import Cal.Doc
import Cal.Format (fmtAbs, fmtDayMon, fmtNative, fmtPct, fmtQty, fmtScale, fmtSigned)
import Cal.Glyphs (Glyphs (..), hbar)
import Cal.Model.Shared (ForecastHint (..), MonthRow (..))
import Cal.Model.Year
import Cal.Theme (Role (..))
import Cal.Types
import Cal.View.Chrome
import Cal.View.Types

dim, plain :: Style
dim = st RDim
plain = st RText

spaces :: Int -> Seg
spaces n = Seg plain (T.replicate (max 0 n) " ")

padSeg :: Int -> Seg -> [Seg]
padSeg n sg@(Seg _ t) = [sg, spaces (n - textW t)]

-- | English ordinal: 1st, 2nd, 3rd, 4th, 11th, 21st ...
ordinal :: Int -> Text
ordinal n = T.pack (show n) <> suffix
  where
    suffix
      | n `mod` 100 `elem` [11, 12, 13] = "th"
      | n `mod` 10 == 1 = "st"
      | n `mod` 10 == 2 = "nd"
      | n `mod` 10 == 3 = "rd"
      | otherwise = "th"

dayOf :: Day -> Int
dayOf d = let (_, _, x) = toGregorian d in x

monAbbr :: Day -> Text
monAbbr = T.pack . formatTime defaultTimeLocale "%b"

yearOf :: Day -> Integer
yearOf d = let (y, _, _) = toGregorian d in y

yearDoc :: RenderCtx -> YearModel -> ViewDoc
yearDoc ctx m = ViewDoc
  { vdTop = top, vdBody = body, vdCursorRow = cursorRow, vdBottom = footerBlock ctx keys legend
  , vdKeys = keys, vdLegend = legend }
  where
    g = rcGlyphs ctx
    f = rcFmt ctx
    s = rcSettings ctx
    today = sToday s
    k = ymKpis m
    title = T.pack (show (yearOf (spStart (ymSpan m))))
    withAvg w t b = case splitAt 2 (bLines b) of
      (hd, tl) -> Block (bWidth b) (hd ++ bLines (str dim (truncEnd g w t)) ++ tl)
    cards = cardRow ctx
      [ \cw -> maybe id (\a -> withAvg cw ("avg " <> fmtAbs f a <> " / month")) (ymAvgIncome m) (incomeCard ctx cw "" k)
      , \cw -> maybe id (\a -> withAvg cw ("avg " <> fmtAbs f a <> " / full month")) (ymAvgExpenseFull m) (expensesCard ctx cw "" k)
      , \cw -> netCard ctx cw k
      , \cw -> fxCard ctx cw k
      , monthsCard ]
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

    -- column widths
    showSyms = rcTier ctx >= Tier120
    nameW = 11
    symW = if showSyms then 12 else 0
    allRows = [ r | q <- ymGroups m, r <- qgMonths q ]
    bw = if ymScale m == Nothing then 0 else rcWidth ctx `div` 3
    avail = rcWidth ctx - (2 + nameW + symW + bw) - 4 - 3
    needFig = maximum (0 : [ textW (fmtAbs f x) | r <- allRows, x <- [mrIncome r, mrExpenses r] ])
    needNet = maximum (0 : [ textW (fmtSigned f (mrNet r)) | r <- allRows ])
    netW0 = min needNet (avail `div` 2)
    figW = min needFig (avail - netW0)
    netW = min needNet (avail - figW)

    -- body: tagged blocks (is the cursor row?), a blank line between groups
    cursorMonth = [ spStart (mrSpan r) | q <- ymGroups m, r <- qgMonths q
                  , spStart (mrSpan r) <= ymCursor m, ymCursor m < spEnd (mrSpan r) ]
    tagged = concat (intersperse [(False, blankB)]
      [ (False, header q) : [ (isCursorRow r, rowBlock r) | r <- qgMonths q ] | q <- ymGroups m ])
    blankB = Block 0 [Line []]
    body = vcat (map snd tagged)
    offsets = scanl (+) 0 [ length (bLines b) | (_, b) <- tagged ]
    cursorRow
      | rcInteractive ctx = lookup True (zip (map fst tagged) offsets)
      | otherwise = Nothing

    header q = line $
      [ Seg (bold plain) ("Q" <> T.pack (show (qgIndex q))), Seg plain "  "
      , Seg (bold plain) (fmtSigned f (qgNet q)), Seg plain "  " ] ++
      case qgStatus q of
        Open -> [Seg dim ("open" <> gSep g <> "to " <> fmtDayMon today)]
        _ -> [Seg dim (maybe "—" fmtPct (qgSpent q) <> " spent")]

    rowBlock r
      | mrStatus r == Future && not (mrHasPostings r) = futureRow r
      | otherwise = vcat [row1 r, row2 r]
    isCursorRow r = rcInteractive ctx && spStart (mrSpan r) `elem` cursorMonth
    nameSegs r =
      let nm = monAbbr (spStart (mrSpan r))
          nmSty = if isCursorRow r then inverse (bold (st RCursor)) else bold plain
          extra = if mrStatus r == Open then " to " <> ordinal (dayOf today) else ""
      in [Seg nmSty nm, Seg dim extra] ++ [spaces (nameW - textW nm - textW extra)]
    symSegs r
      | not showSyms = []
      | otherwise = padSeg symW (Seg dim (truncEnd g (symW - 1) (T.unwords [ maybe c id (symbolOf c) | c <- mrCommodities r ])))
    barSegs role frac = case frac of
      Nothing -> [spaces bw]
      Just x -> let b = hbar g bw x in [Seg (st role) b, spaces (bw - textW b)]
    fig :: Int -> Style -> Text -> Seg
    fig w sty t = let t' = truncEnd g w t in Seg sty (T.replicate (w - textW t') " " <> t')
    row1 r = line $
      [spaces 2] ++ nameSegs r ++ symSegs r ++ barSegs RInc (mrInFrac r)
      ++ [spaces 2, fig figW plain (fmtAbs f (mrIncome r)), spaces 2
         , fig netW (if mrOverspent r then st RExp else plain) (fmtSigned f (mrNet r))]
      ++ [Seg (st RExp) (" " <> gOverspent g) | mrOverspent r]
      ++ [Seg dim (" " <> gOpen g) | mrStatus r == Open]
    row2 r = line $
      [spaces (2 + nameW + symW)] ++ barSegs RExp (mrOutFrac r)
      ++ [spaces 2, fig figW plain (fmtAbs f (mrExpenses r))]
    futureRow r = line $
      [spaces 2] ++ nameSegs r ++ symSegs r
      ++ [Seg dim "—  no postings yet"]
      ++ maybe [] (\h -> [Seg dim ("  " <> truncEnd g room (hintText h))]) (mrHint r)
      where room = rcWidth ctx - (2 + nameW + symW + textW ("—  no postings yet" :: Text) + 2)
    hintText h = "forecast " <> fhDescription h <> " " <> fmtNative f (fhNative h)
      <> maybe "" (\v -> " ≈ " <> fmtQty f (negate v)) (fhValued h)
      <> (if fhMore h > 0 then " +" <> T.pack (show (fhMore h)) else "")

    keys = [("↑↓←→", "month"), ("⏎", "open month"), ("c", "currency"), ("v", "rates at"), ("/", "filter account"), ("q", "quit")]
    legend = line $
      [ Seg dim "bars: ", Seg (st RInc) (gLegendBar g), Seg dim " in ", Seg (st RExp) (gLegendBar g), Seg dim " out" ]
      ++ [ Seg dim (gSep g <> "scale " <> fmtScale f sc) | Just sc <- [ymScale m] ]
      ++ [ Seg dim (gSep g <> "symbols = currencies used") ]
