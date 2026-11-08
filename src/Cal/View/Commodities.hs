-- | The Commodities face: holdings per commodity with price, value, share,
-- gain and source, the selected commodity's price chart and the explainer. Pure.
module Cal.View.Commodities (commoditiesDoc) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (toGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Numeric (showFFloat)
import Hledger (mixed, nullamt)
import qualified Hledger as H

import Cal.Currency (chipLabel, codeOf, symbolOf)
import Cal.Doc
import Cal.Format
import Cal.Glyphs (BoxChars (..), Glyphs (..), sparkline, vbar)
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

commoditiesDoc :: RenderCtx -> CommoditiesModel -> ViewDoc
commoditiesDoc ctx m = ViewDoc
  { vdTop = top, vdBody = body, vdCursorRow = cursorRow, vdCursorLast = cursorLast, vdStretch = Nothing
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
    -- chart axis labels: thousands as k, millions as M, else as 'priceT'
    axisT q
      | abs q >= 1000000 = T.pack (showFFloat (Just 1) (realToFrac q / 1000000 :: Double) "") <> "M"
      | abs q >= 10000 = T.pack (show (round (q / 1000) :: Integer)) <> "k"
      | otherwise = priceT q
    pctSeg x = let t = fmtDeltaPct f x in Seg (signStyle g t) t
    priced r = valued && not (crIsBase r)
    one t = [Seg plain t]
    dimD = [Seg dim dash]

    bx = gBox g
    -- a security's quote currency: what it is priced via, else the base
    quoteSym r = case crPrice r of
      Just i | valued -> Just $ case riSource i of
        SrcVia x -> fromMaybe x (symbolOf x)
        _ -> baseSym
      _ -> Nothing
    commCell r =
      Seg plain (chipLabel (crCommodity r))
        : [ Seg dim " base" | crIsBase r ]
        ++ [ Seg (st RWarn) (" " <> gStale g) | isJust (crStale r) ]
        ++ [ Seg dim (" " <> q) | isSecurity (crCommodity r), Just q <- [quoteSym r] ]
    -- held and changed amounts in the row's unit word, else with its own symbol
    units r q = case crUnit r of
      Just u -> fmtUnits f u (crCommodity r) q
      Nothing -> fmtNative f (amt (crCommodity r) q)
    heldCell r = one (units r (crHeld r))
    changeCell r
      | crChange r == 0 = [Seg dim "0"]
      | otherwise =
          let t = (if crChange r > 0 then "+" else "") <> units r (crChange r)
          in [Seg (signStyle g t) t]
    priceCell r
      | crIsBase r = one "1"
      | otherwise = case crPrice r of
          Just i | valued -> one (fmtRate (riRate i))
          _ -> dimD
    -- under the base price, dimmed: the price in the commodity it is priced via
    priceVia r = case (crViaPrice r, crPrice r) of
      (Just (x, q), Just _) | valued -> Just [Seg dim (fmtNative f (amt x q))]
      _ -> Nothing
    deltaCell r = case crDeltaPct r of
      Just x | priced r -> [pctSeg x]
      _ -> dimD
    sparkCell r
      | crIsBase r = [Seg dim "base"]
      | not valued = dimD
      | otherwise = [Seg plain (sparkline g (map (fmap realToFrac) (crSpark r)))]
    valueCell r = case crValue r of
      Just q | valued -> one (valueT q)
      _ -> dimD
    -- share: a thin bar scaled to the largest share, then the percentage
    shareBarW = 6
    maxShare = maximum (0 : [x | r <- rows, Just x <- [crShare r]])
    pctW = maximum (0 : [textW (fmtPct f x) | r <- rows, Just x <- [crShare r]])
    shareCell bar r = case crShare r of
      Just x | valued ->
        [ sg | bar, sg <- thinBar ctx RDim shareBarW (if maxShare > 0 then x / maxShare else 0) ++ [spaces 1] ]
          ++ [Seg dim (T.justifyRight pctW ' ' (fmtPct f x))]
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

    allCols bar =
      [ Col CComm "commodity" AL commCell, Col CHeld "held" AR heldCell
      , Col CChange thisWord AR changeCell, Col CPrice (withBase "price") AR priceCell
      , Col CDelta deltaHead AR deltaCell, Col CSpark "12 months" AL sparkCell
      , Col CValue (withBase "value") AR valueCell, Col CShare "share" AL (shareCell bar)
      , Col CGain "unreal. gain" AR gainCell, Col CSource "price source" AL sourceCell ]
    tierDrops
      | tier == Tier80 = [CSource, CSpark, CGain]
      | tier <= Tier120 = [CSource]
      | otherwise = []
    -- the table sits in a box: a border and a space of padding on each side
    iw = w - 4
    -- further columns dropped, in order, while the table is too wide
    needDrops = [CSource, CSpark, CGain, CChange, CDelta, CShare]
    colWidth c = maximum (textW (cHead c) : map (segsW . cCell c) rows
      ++ [segsW ss | cId c == CPrice, Just ss <- map priceVia rows]
      ++ [segsW (totalCell c) | cId c `elem` [CValue, CGain, CSource]])
    naturalW cs = sum (map colWidth cs) + 2 * (length cs - 1)
    fitCols cs = case [d | d <- needDrops, d `elem` map cId cs] of
      (d : _) | naturalW cs > iw -> fitCols (filter ((/= d) . cId) cs)
      _ -> cs
    chosen bar = fitCols (filter ((`notElem` tierDrops) . cId) (allCols bar))
    -- the share bar only where it costs no column
    shareBar = map cId (chosen True) == map cId (chosen False)
    cols = chosen shareBar
    widths = map colWidth cols
    nCols = length cols
    gap = max 2 (min 6 ((iw - sum widths) `div` max 1 (nCols - 1)))
    hasCol i = i `elem` map cId cols

    cellSegs a cw ss = case a of
      AL -> ss ++ [spaces (cw - segsW ss)]
      AR -> spaces (cw - segsW ss) : ss
    rowSegs :: [[Seg]] -> [Seg]
    rowSegs cells = concat (zipWith3 (\(c, cw) ss i -> (if i > 0 then [spaces gap] else []) ++ cellSegs (cAlign c) cw ss)
                                     (zip cols widths) cells [0 :: Int ..])
    headLine = rowSegs [ [Seg dim (cHead c)] | c <- cols ]
    dataLine r = rowSegs [ cCell c r | c <- cols ]
    -- a price via another commodity puts that price on a second line
    viaLine r = case priceVia r of
      Just ss | hasCol CPrice -> [rowSegs [ if cId c == CPrice then ss else [] | c <- cols ]]
      _ -> []

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

    -- * The table box: a rule between rows, group headers on the panel tint
    edgeS = Seg (st RRule) (T.singleton (bxV bx))
    inBox ss = Line ([edgeS, spaces 1] ++ ss ++ [spaces (iw - segsW ss + 1), edgeS])
    hRule l r = Line [Seg (st RRule) (T.singleton l <> T.replicate (w - 2) (T.singleton (bxH bx)) <> T.singleton r)]
    groupLine name = Line [edgeS, Seg (onBg RPanel 100 dim) (" " <> name <> T.replicate (iw + 1 - textW name) " "), edgeS]
    group name rs =
      [ (False, [groupLine name]) | not (null rs) ]
        ++ [ (isCursor r, map inBox (dataLine r : viaLine r)) | r <- rs ]
    items :: [(Bool, [Line])]
    items =
      [(False, [inBox headLine])]
        ++ group "CURRENCIES" (cmCurrencies m)
        ++ group "SECURITIES" (cmSecurities m)
        ++ [(False, map inBox totalLines)]
    -- the body line of each item's first line: one rule above each item
    starts = scanl (\o (_, ls) -> o + length ls + 1) 1 items
    table = Block w ([hRule (bxTL bx) (bxTR bx)] ++ intercalate [hRule (bxLT bx) (bxRT bx)] (map snd items) ++ [hRule (bxBL bx) (bxBR bx)])
    -- the cursor row: its first and last lines
    cursorSpan = listToMaybe [ (o, o + length ls - 1) | ((True, ls), o) <- zip items starts ]
    body = case cursorSpan of
      Just (o, e) -> cursorRect (o - 1, 0) (e + 1, w - 1) table
      Nothing -> table
    cursorRow = fst <$> cursorSpan
    cursorLast = (+ 1) . snd <$> cursorSpan

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
    detailLines dI cd = vcat ([heading] ++ (if null present then [] else blankB : chart ++ [axis]) ++ splitL)
      where
        c = cdCommodity cd
        kind = case [crUnit r | r <- rows, crCommodity r == c] of
          (Just "sh" : _) -> "per share"
          (Just u : _) -> "per " <> u
          _ -> "per unit"
        left = codeOf c <> (if valued then " in " <> baseSym else "") <> gSep g <> kind <> gSep g <> "12 months"
        rangeT = maybe dash priceT (cdStart cd) <> " " <> gArrow g <> " " <> maybe dash priceT (cdEnd cd) <> gSep g
        pctS = maybe (Seg dim dash) pctSeg (cdPct cd)
        rightW = textW rangeT + segsW [pctS]
        heading
          | textW left + 2 + rightW <= dI =
              line [Seg (bold plain) left, spaces (dI - textW left - rightW), Seg dim rangeT, pctS]
          | otherwise = vcat [str (bold plain) (truncEnd g dI left), line [Seg dim rangeT, pctS]]
        -- a bar per month-end. The top tick is the highest price rounded up
        -- and the floor the lowest less a twentieth of the range rounded
        -- down, both to a tenth of the range's order of magnitude (about two
        -- significant figures of the range), so the highest bar reaches the
        -- top row and the lowest still shows; the middle tick is exact
        vals = map (fmap realToFrac . snd) (cdSeries cd) :: [Maybe Double]
        present = [v | Just v <- vals]
        (hi, lo) = if null present then (0, 0) else (maximum present, minimum present)
        range' = if hi > lo then hi - lo else max 1 (abs hi / 10)
        unitV = 10 ^^ (floor (logBase 10 range') - 1 :: Integer) :: Double
        topV = fromIntegral (ceiling (hi / unitV) :: Integer) * unitV
        floorV = fromIntegral (floor ((lo - range' / 20) / unitV) :: Integer) * unitV
        frac v = (v - floorV) / (topV - floorV)
        chartH = 8
        labels = map (axisT . realToFrac) [topV, (topV + floorV) / 2, floorV]
        labelW = maximum (map textW labels)
        labelAt i
          | i == 0 = labels !! 0
          | i == chartH `div` 2 = labels !! 1
          | i == chartH - 1 = labels !! 2
          | otherwise = ""
        -- one slot per month, the spare cells spread over the first slots
        barsW = max 1 (dI - labelW - 2)
        nPts = length vals
        (slotW, spare) = barsW `divMod` max 1 nPts
        slotWs = [ slotW + (if i < spare then 1 else 0) | i <- [0 .. nPts - 1] ]
        barW = max 1 (min 6 (slotW - max 1 (slotW `div` 4)))
        lead sw = (sw - barW) `div` 2
        lastI = nPts - 1
        -- the last bar in the text colour, the others faded; without colour
        -- the others are whole shaded cells
        barStyle i = if i == lastI then plain else st RFaded
        shadeG i
          | i == lastI || colourOn ctx = g
          | otherwise = g { gFull = if null (gColEighths g) then ':' else '▒', gColEighths = [] }
        column i v = case v of
          Just x -> vbar (shadeG i) chartH (frac x)
          Nothing -> replicate chartH ' '
        cols' = zip3 [0 :: Int ..] slotWs (zipWith column [0 ..] vals)
        -- a month with no price: one dim dot on the floor
        slot r (i, sw, col)
          | isJust (vals !! i) =
              [spaces (lead sw), Seg (barStyle i) (T.replicate barW (T.singleton (col !! r))), spaces (sw - lead sw - barW)]
          | r == chartH - 1 = [spaces (mid sw), Seg dim (T.singleton (gSparkMissing g)), spaces (sw - mid sw - 1)]
          | otherwise = [spaces sw]
        mid sw = lead sw + (barW - 1) `div` 2
        axisS = Seg (st RRule) (T.singleton (bxV bx))
        chart = [ line ([Seg dim (T.justifyRight labelW ' ' (labelAt r)), Seg plain " ", axisS] ++ concatMap (slot r) cols')
                | r <- [0 .. chartH - 1] ]
        initial d = T.take 1 (T.pack (formatTime defaultTimeLocale "%b" d))
        axis = line (spaces (labelW + 2) : concat
          [ [spaces (mid sw), Seg (if i == lastI then bold plain else dim) (initial d), spaces (sw - mid sw - 1)]
          | ((i, sw, _), (d, _)) <- zip cols' (cdSeries cd) ])
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
