-- | The Commodities face: holdings per commodity with price, value, share,
-- gain and source, the selected commodity's price chart and the explainer. Pure.
module Cal.View.Commodities (commoditiesDoc) where

import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (toGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Hledger (mixed, nullamt)
import qualified Hledger as H

import Cal.Currency (chipLabel, codeOf, symbolOf)
import Cal.Doc
import Cal.Format
import Cal.Glyphs (BoxChars (..), Glyphs (..), lineChart, sparkline)
import Cal.Model.Commodities
import Cal.Period (isoWeek, quarterOf)
import Cal.Prices (RateInfo (..), RateSource (..))
import Cal.Theme (Role (..))
import Cal.Types
import Cal.View.Chrome
import Cal.View.Types

data Align = AL | AR deriving Eq

-- | A table column: id, heading, alignment and one cell per row.
data Col = Col { cId :: !ColId, cHead :: !Text, cAlign :: !Align, cCell :: CommodityRow -> [Seg] }

data ColId = CComm | CHeld | CChange | CPrice | CDelta | CSpark | CValue | CShare | CGain | CSource
  deriving (Eq, Show)

-- | Word-wrap @t@ into lines of at most @n@ columns; continuation lines get @indent@.
wrapWords :: Int -> Int -> Text -> [Text]
wrapWords n indent t = case T.words t of
  [] -> [""]
  (x : xs) -> go x xs
  where
    pre = T.replicate indent " "
    go cur [] = [cur]
    go cur (y : ys)
      | textW cur + 1 + textW y <= n = go (cur <> " " <> y) ys
      | otherwise = cur : go (pre <> y) ys

commoditiesDoc :: RenderCtx -> CommoditiesModel -> ViewDoc
commoditiesDoc ctx m = ViewDoc
  { vdTop = top, vdBody = body, vdCursorRow = cursorRow
  , vdBottom = panels
  , vdKeys = keys, vdLegend = legend }
  where
    g = rcGlyphs ctx
    f = rcFmt ctx
    s = rcSettings ctx
    w = rcWidth ctx
    tier = rcTier ctx
    interactive = rcInteractive ctx
    valued = isJust (fBase f)
    dash = gDash g
    blankB = Block 0 [Line []]
    sepCh = T.strip (gSep g)
    baseSym = case fBase f of
      Just b -> fromMaybe b (symbolOf b)
      Nothing -> ""
    withBase t = if valued then t <> " " <> baseSym else t

    -- * Top
    start = spStart (cmSpan m)
    periodLabel = case sCommInterval s of
      IYear -> let (y, _, _) = toGregorian start in T.pack (show y)
      IQuarter -> let (y, _, _) = toGregorian start in "Q" <> T.pack (show (quarterOf start)) <> " " <> T.pack (show y)
      IMonth -> fmtMonthYear start
      IWeek -> let (y, wk) = isoWeek start in "Week " <> T.pack (show wk) <> " " <> T.pack (show y)
    title = "Commodities  " <> periodLabel <> gSep g <> "held at " <> fmtDayMon (cmHeldAt m)
    top = topChrome ctx title (cmBar m) []

    -- * Table cells
    (thisWord, deltaWord) = case sCommInterval s of
      IYear -> ("this year", "yr")
      IQuarter -> ("this quarter", "qtr")
      IMonth -> ("this month", "mo")
      IWeek -> ("this week", "wk")
    deltaHead = gDelta g <> " " <> deltaWord
    rows = cmCurrencies m ++ cmSecurities m
    isSecurity c = c `elem` map crCommodity (cmSecurities m)
    selected = drop (cmSelected m) rows
    isCursor r = interactive && take 1 selected == [r]
    amt c q = mixed [nullamt { H.acommodity = c, H.aquantity = q }]
    -- a base-style figure without the leading "+"
    unsigned t = fromMaybe t (T.stripPrefix "+" t)
    valueT = unsigned . fmtQty f
    -- prices in charts and the detail heading: base style from 1,000 up
    priceT q = if abs q >= 1000 then valueT q else fmtRate q
    pctSeg x = let t = fmtDeltaPct f x in Seg (signStyle g t) t
    priced r = valued && not (crIsBase r)
    one t = [Seg plain t]
    dimD = [Seg dim dash]

    commCell r =
      Seg (if isCursor r then inverse (bold (st RCursor)) else plain) (chipLabel (crCommodity r))
        : [ Seg (st RWarn) (" " <> gStale g) | isJust (crStale r) ]
    heldCell r = one (fmtNative f (amt (crCommodity r) (crHeld r)))
    changeCell r
      | crChange r == 0 = [Seg dim "0"]
      | otherwise =
          let t = (if crChange r > 0 then "+" else "") <> fmtNative f (amt (crCommodity r) (crChange r))
          in [Seg (signStyle g t) t]
    priceCell r
      | crIsBase r = [Seg dim ("1 " <> dash <> " base")]
      | otherwise = case crPrice r of
          Just i | valued -> one (fmtRate (riRate i))
          _ -> dimD
    deltaCell r = case crDeltaPct r of
      Just x | priced r -> [pctSeg x]
      _ -> dimD
    sparkCell r
      | crIsBase r = []
      | not valued = dimD
      | otherwise = [Seg plain (sparkline g (map (fmap realToFrac) (crSpark r)))]
    valueCell r = case crValue r of
      Just q | valued -> one (valueT q)
      _ -> dimD
    shareCell r = case crShare r of
      Just x | valued -> [Seg dim (fmtPct f x)]
      _ -> dimD
    gainCell r = case crGain r of
      Just q | priced r -> let t = fmtQty f q in [Seg (signStyle g t) t]
      _ -> dimD
    sourceCell r
      | not (priced r) = dimD
      | otherwise = case (crPrice r, crStale r) of
          (Nothing, _) -> [Seg (st RWarn) "no price"]
          (Just i, Just n) -> [Seg (st RWarn) ("stale" <> gSep g <> fmtDayMon (riDate i) <> " (" <> T.pack (show n) <> " d)")]
          (Just i, Nothing) -> [Seg dim $ case riSource i of
            SrcDeclared -> "P" <> gSep g <> fmtDayMon (riDate i)
            SrcInferred -> "inferred" <> gSep g <> fmtDayMon (riDate i)
            SrcVia x -> "via " <> codeOf x <> gSep g <> fmtDayMon (riDate i)]

    allCols =
      [ Col CComm "commodity" AL commCell, Col CHeld "held" AR heldCell
      , Col CChange thisWord AR changeCell, Col CPrice (withBase "price") AR priceCell
      , Col CDelta deltaHead AR deltaCell, Col CSpark "12 months" AL sparkCell
      , Col CValue (withBase "value") AR valueCell, Col CShare "share" AR shareCell
      , Col CGain "unreal. gain" AR gainCell, Col CSource "price source" AL sourceCell ]
    tierDrops
      | tier == Tier80 = [CSource, CSpark, CGain]
      | tier <= Tier120 = [CSource]
      | otherwise = []
    -- further columns dropped, in order, while the table is too wide
    needDrops = [CSource, CSpark, CGain, CChange, CDelta, CShare]
    colWidth c = maximum (textW (cHead c) : map (segsW . cCell c) rows
      ++ [segsW (totalCell c) | cId c `elem` [CValue, CGain, CSource]])
    naturalW cs = sum (map colWidth cs) + 2 * (length cs - 1)
    fitCols cs = case [d | d <- needDrops, d `elem` map cId cs] of
      (d : _) | naturalW cs > w -> fitCols (filter ((/= d) . cId) cs)
      _ -> cs
    cols = fitCols (filter ((`notElem` tierDrops) . cId) allCols)
    widths = map colWidth cols
    nCols = length cols
    gap = max 2 (min 4 ((w - sum widths) `div` max 1 (nCols - 1)))
    hasCol i = i `elem` map cId cols

    cellSegs a cw ss = case a of
      AL -> ss ++ [spaces (cw - segsW ss)]
      AR -> spaces (cw - segsW ss) : ss
    rowSegs :: [[Seg]] -> [Seg]
    rowSegs cells = concat (zipWith3 (\(c, cw) ss i -> (if i > 0 then [spaces gap] else []) ++ cellSegs (cAlign c) cw ss)
                                     (zip cols widths) cells [0 :: Int ..])
    tableW = sum widths + gap * (nCols - 1)
    ruleLine = [Seg (st RRule) (T.replicate (min w tableW) (T.singleton (bxH (gBox g))))]
    headLine = rowSegs [ [Seg dim (cHead c)] | c <- cols ]
    dataLine r = rowSegs [ cCell c r | c <- cols ]
    groupLines name rs
      | null rs = []
      | otherwise = ([Seg dim name], False) : [ (dataLine r, isCursor r) | r <- rs ]

    -- totals: counts span the columns between commodity and value
    plural n one' many = T.pack (show n) <> " " <> (if n == 1 then one' else many)
    counts = plural (length rows) "commodity" "commodities" <> "  " <> T.pack (show (cmUnpriced m)) <> " unpriced"
    staleT = plural (cmStaleCount m) "stale price" "stale prices"
    spanIds = takeWhile (/= CValue) (drop 1 (map cId cols))
    spanW = sum [cw | (c, cw) <- zip cols widths, cId c `elem` spanIds] + gap * (length spanIds - 1)
    staleInSpan = not (hasCol CSource) && textW counts + 2 + textW staleT <= spanW
    spanText = if staleInSpan then counts <> "  " <> staleT else counts
    totalCell c = case cId c of
      CComm -> [Seg (bold plain) "total"]
      CValue -> case cmTotalValue m of
        Just q | valued -> [Seg (bold plain) (valueT q)]
        _ -> dimD
      CGain -> case cmTotalGain m of
        Just q | valued -> let t = fmtQty f q in [Seg (bold (signStyle g t)) t]
        _ -> dimD
      CSource -> [Seg (if cmStaleCount m > 0 then st RWarn else dim) staleT]
      _ -> []
    (firstCol, firstW) = case zip cols widths of
      (x : _) -> x
      [] -> (Col CComm "" AL (const []), 0)
    totalLines =
      let afterSpan = dropWhile ((`elem` (CComm : spanIds)) . cId . fst) (zip cols widths)
          tail' = concat [ spaces gap : cellSegs (cAlign c) cw (totalCell c) | (c, cw) <- afterSpan ]
          spanSegs = [Seg dim spanText, spaces (spanW - textW spanText)]
          main = cellSegs AL firstW (totalCell firstCol) ++ [spaces gap] ++ spanSegs ++ tail'
          staleWarn = if cmStaleCount m > 0 then st RWarn else dim
      in [main] ++ [ [spaces (firstW + gap), Seg staleWarn staleT] | not (hasCol CSource), not staleInSpan ]

    tagged =
      [(headLine, False), (ruleLine, False)]
        ++ groupLines "CURRENCIES" (cmCurrencies m)
        ++ groupLines "SECURITIES" (cmSecurities m)
        ++ [(ruleLine, False)]
        ++ [ (l, False) | l <- totalLines ]
    body = padTo w (vcat [ line ss | (ss, _) <- tagged ])
    cursorRow
      | interactive = lookup True (zip (map snd tagged) [0 ..])
      | otherwise = Nothing

    -- * Bottom panels
    full = tier == TierFull
    box t bw b = boxed g t bw (hcat 0 [str plain " ", padTo (bw - 4) b])
    pad h b = vcat [b, Block 0 (replicate (h - length (bLines b)) (Line []))]
    detailW = if full then (w - 2) * 23 `div` 43 else w
    explW = if full then w - 2 - detailW else w
    detailB = fmap (detailLines (detailW - 4)) (cmDetail m)
    explB = explainerLines (explW - 4)
    explTitle = Just (" How these numbers are made ")
    panels = case detailB of
      Just d
        | full ->
            let h = max (length (bLines d)) (length (bLines explB))
            in hcat 2 [box Nothing detailW (pad h d), box explTitle explW (pad h explB)]
        | otherwise -> vcat [box Nothing detailW d, blankB, box explTitle explW explB]
      Nothing -> box explTitle explW explB

    -- the chart only when the commodity has a price in the base
    detailLines dI cd = vcat ([heading] ++ (if null present then [] else chart ++ [axis]) ++ splitL)
      where
        c = cdCommodity cd
        kind = if isSecurity c then "per share" else "per unit"
        left = codeOf c <> (if valued then " in " <> baseSym else "") <> gSep g <> kind <> gSep g <> "12 months"
        rangeT = maybe dash priceT (cdStart cd) <> " " <> gArrow g <> " " <> maybe dash priceT (cdEnd cd) <> gSep g
        pctS = maybe (Seg dim dash) pctSeg (cdPct cd)
        rightW = textW rangeT + segsW [pctS]
        heading
          | textW left + 2 + rightW <= dI =
              line [Seg (bold plain) left, spaces (dI - textW left - rightW), Seg dim rangeT, pctS]
          | otherwise = vcat [str (bold plain) (truncEnd g dI left), line [Seg dim rangeT, pctS]]
        vals = map snd (cdSeries cd)
        present = [v | Just v <- vals]
        (hi, lo) = if null present then (0, 0) else (maximum present, minimum present)
        labels
          | null present = ["", "", ""]
          | otherwise = map priceT [hi, (hi + lo) / 2, lo]
        labelW = maximum (map textW labels)
        chartW = max 1 (dI - labelW - 1)
        chartRows = lineChart g chartW 8 (map (fmap realToFrac) vals)
        labelAt i = case i of
          0 -> labels !! 0
          4 -> labels !! 1
          7 -> labels !! 2
          _ -> ""
        chart = [ line [Seg dim (T.justifyRight labelW ' ' (labelAt i)), Seg plain " ", Seg (st RInc) r]
                | (i, r) <- zip [0 :: Int ..] chartRows ]
        nPts = length (cdSeries cd)
        initial d = T.take 1 (T.pack (formatTime defaultTimeLocale "%b" d))
        xs = [ (if nPts == 0 then 0 else (i * chartW + nPts - 1) `div` nPts, initial d)
             | (i, (d, _)) <- zip [0 ..] (cdSeries cd) ]
        axisText = T.pack [ maybe ' ' T.head (lookup x xs) | x <- [0 .. chartW - 1] ]
        axis = line [spaces (labelW + 1), Seg dim axisText]
        splitL = case cdSplit cd of
          Just (x, own, fx, tot) | valued ->
            let xs' = fromMaybe x (symbolOf x)
                segs = [ Seg dim ("share price in " <> xs' <> " "), pctSeg own
                       , Seg dim ("  " <> gTimes g <> " " <> xs' <> " " <> gArrow g <> " " <> baseSym <> " "), pctSeg fx
                       , Seg dim ("  = in " <> baseSym <> " "), pctSeg tot ]
            in [blankB, if segsW segs <= dI then line segs else str dim (truncEnd g dI (T.concat [t | Seg _ t <- segs]))]
          _ -> []

    explainerLines eI = vcat (map (str dim) bullets ++ map (str (st RWarn)) staleNotes)
      where
        bullet t = wrapWords eI 2 (sepCh <> " " <> t)
        bullets = concatMap bullet
          [ "A commodity is anything the journal counts: currencies, shares, units. Each one is valued in the base through any chain of prices (VOO "
              <> gArrow g <> " USD " <> gArrow g <> " NGN)."
          , "Prices come from P directives (e.g. a daily price feed) first. Otherwise they're inferred from transaction costs (@ / @@)."
          , "Unrealized gain = value now " <> gMinus g <> " what was paid (the @ cost)."
          , "A security priced only by inference moves only when you trade it. Add P directives to follow the market." ]
        staleNotes = concat
          [ wrapWords eI 2 (gStale g <> " " <> codeOf (crCommodity r) <> " has no price since " <> fmtDayMon (riDate i)
                              <> ", so a " <> T.pack (show n) <> "-day-old rate is used.")
          | r <- cmCurrencies m, Just n <- [crStale r], Just i <- [crPrice r] ]

    -- * Footer
    keys = [(gKeyUpDown g, "commodity"), (gKeyEnter g, "value in this"), ("[ ]", "period"), ("v", "rates at"), ("/", "filter account"), ("q", "quit")]
    legend = line [Seg dim "share = part of total value held"]
