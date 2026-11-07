-- | The Commodities face model: holdings per commodity with price, value,
-- share, gain and a 12-month price series.
module Cal.Model.Commodities
  ( CommodityRow (..)
  , CommodityDetail (..)
  , CommoditiesModel (..)
  , commoditiesModel
  ) where

import Cal.Journal (isForecastTxn)
import Cal.Model.Common
import Cal.Model.Shared
import Cal.Period (faceSpan, intervalSpan, lastDay, prevSpan, spanStatus)
import Cal.Prices (Prices (..), RateInfo (..), RateSource (..), rateInfo, rateOn, staleAge)
import Cal.Types
import Cal.Valuation (baseQty, foreignPart)
import Data.List (nub, sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Time.Calendar (addGregorianMonthsClip)
import Hledger hiding (Interval (..), Status)

data CommodityRow = CommodityRow
  { crCommodity :: !CommoditySymbol, crIsBase :: !Bool, crHeld, crChange :: !Quantity
  , crPrice :: !(Maybe RateInfo), crDeltaPct :: !(Maybe Double), crSpark :: ![Maybe Quantity]
  , crValue :: !(Maybe Quantity), crShare :: !(Maybe Double), crGain :: !(Maybe Quantity)
  , crStale :: !(Maybe Integer) }
  deriving (Eq, Show)

-- | cdSplit = (via, own-price change, via→base change, total change).
data CommodityDetail = CommodityDetail
  { cdCommodity :: !CommoditySymbol, cdSeries :: ![(Day, Maybe Quantity)]
  , cdStart, cdEnd :: !(Maybe Quantity), cdPct :: !(Maybe Double)
  , cdSplit :: !(Maybe (CommoditySymbol, Double, Double, Double)) }
  deriving (Eq, Show)

data CommoditiesModel = CommoditiesModel
  { cmSpan :: !Span, cmHeldAt :: !Day, cmBar :: !CurrencyBar
  , cmCurrencies, cmSecurities :: ![CommodityRow]
  , cmTotalValue, cmTotalGain :: !(Maybe Quantity), cmUnpriced, cmStaleCount :: !Int
  , cmSelected :: !Int, cmDetail :: !(Maybe CommodityDetail) }
  deriving (Eq, Show)

commoditiesModel :: Env -> Cursor -> CommoditiesModel
commoditiesModel env cursor = CommoditiesModel
  { cmSpan = span', cmHeldAt = heldAt, cmBar = currencyBar env span'
  , cmCurrencies = currRows, cmSecurities = secRows
  , cmTotalValue = total, cmTotalGain = totalGain
  , cmUnpriced = if isJust base then length (filter (isNothing . crValue) allRows) else 0
  , cmStaleCount = length (filter (isJust . crStale) allRows)
  , cmSelected = selected
  , cmDetail = detail . crCommodity <$> listToMaybe (drop selected allRows) }
  where
    s = envSettings env
    j = envJournal env
    prices = envPrices env
    base = envBase env
    span' = faceSpan FaceCommodities (sCommInterval s) (sFocus s)
    heldAt = case spanStatus (sToday s) span' of
      Open -> sToday s
      _ -> lastDay span'
    -- staleness is never judged as of a future day
    staleAsOf = min heldAt (sToday s)

    -- query-matched balance-sheet postings up to heldAt
    holdingPs =
      [ p | p <- journalPostings j
          , postingDate p <= heldAt
          , journalAccountType j (paccount p) `elem` map Just [Asset, Cash, Liability]
          , sForecast s || maybe True (not . isForecastTxn) (ptransaction p)
          , matchesPostingExtra (journalAccountType j) (sQuery s) p ]
    qtys ps = M.fromListWith (+)
      [ (acommodity a, aquantity a) | p <- ps, a <- amounts (mixedAmountStripCosts (pamount p)) ]
    inSpanPs = filter (\p -> postingDate p >= spStart span' && postingDate p < spEnd span') holdingPs
    held = qtys holdingPs
    changed = qtys inSpanPs
    commodities =
      [ c | c <- nub [acommodity a | p <- holdingPs, a <- amounts (pamount p)]
          , M.findWithDefault 0 c held /= 0 || M.member c changed ]

    -- the 12 month-ends ending with heldAt's month
    monthEnds =
      let m0 = spStart (intervalSpan IMonth heldAt)
      in [lastDay (intervalSpan IMonth (addGregorianMonthsClip (-k) m0)) | k <- [11, 10 .. 0]]
    priceIn c d = case base of
      Just b | c /= b -> rateOn prices c b d
      _ -> Nothing
    series c = [(d, priceIn c d) | d <- monthEnds]

    valueOf b c q
      | c == b = Just q
      | otherwise = valueAt b heldAt (nullamt { acommodity = c, aquantity = q })
    -- base quantity of an amount valued on a day; Nothing when unpriced
    valueAt b d a =
      let v = mixedAmountValueAtDate (prOracle prices) (prStyles prices) (Just b) d (mixed [a])
      in if all ((== 0) . aquantity) (amounts (foreignPart b v)) then Just (baseQty b v) else Nothing

    -- (Σ base cost, Σ quantity) of acquisitions with a cost
    acquisitions b c = do
      let acqs =
            [ (postingDate p, a, cost)
            | p <- holdingPs
            , journalAccountType j (paccount p) `elem` map Just [Asset, Cash]
            , a <- amounts (pamount p), acommodity a == c, aquantity a > 0
            , Just cost <- [acost a] ]
          costAmt a (UnitCost u) = u { aquantity = aquantity u * aquantity a }
          costAmt _ (TotalCost t) = t { aquantity = abs (aquantity t) }
      if null acqs then Nothing else do
        costs <- mapM (\(d, a, cost) -> valueAt b d (costAmt a cost)) acqs
        pure (sum costs, sum [aquantity a | (_, a, _) <- acqs])

    mkRow c =
      let h = M.findWithDefault 0 c held
          isBase = Just c == base
          info = case base of
            Just b | not isBase -> rateInfo prices c b heldAt
            _ -> Nothing
          prev = priceIn c (lastDay (prevSpan (sCommInterval s) span'))
          stale = info >>= staleAge (sStaleDays s) staleAsOf
          delta = case (info, stale, prev) of
            (Just i, Nothing, Just p) | p /= 0 -> Just (realToFrac (riRate i) / realToFrac p - 1)
            _ -> Nothing
          val = base >>= \b -> valueOf b c h
          gain = case base of
            Just b | not isBase -> do
              v <- val
              (cost, q) <- acquisitions b c
              if q == 0 then Nothing else Just (v - h * cost / q)
            _ -> Nothing
      in CommodityRow
           { crCommodity = c, crIsBase = isBase, crHeld = h
           , crChange = M.findWithDefault 0 c changed
           , crPrice = info, crDeltaPct = delta, crSpark = map snd (series c)
           , crValue = val, crShare = Nothing, crGain = gain
           , crStale = stale }
    rows0 = map mkRow commodities
    total = case base of
      Just _ -> Just (sum (mapMaybe crValue rows0))
      Nothing -> Nothing
    withShare r = r { crShare = do
      t <- total
      v <- crValue r
      if t == 0 then Nothing else Just (realToFrac v / realToFrac t) }
    rows = map withShare rows0
    rowFor c = filter ((== c) . crCommodity) rows
    choices = currencyChoices env
    currRows = concatMap rowFor choices
    secRows = sortBy (comparing (Down . fromMaybe 0 . crValue))
      [r | r <- rows, crCommodity r `notElem` choices]
    allRows = currRows ++ secRows
    totalGain = case mapMaybe crGain rows of
      [] -> Nothing
      gs -> Just (sum gs)
    selected = max 0 (min (length allRows - 1) (cuCommodity cursor))

    detail c =
      let ser = series c
          valued = [(d, v) | (d, Just v) <- ser]
          firstV = listToMaybe valued
          lastV = listToMaybe (reverse valued)
          pct = case (firstV, lastV) of
            (Just (_, a), Just (_, z)) | a /= 0 -> Just (realToFrac z / realToFrac a - 1)
            _ -> Nothing
          change f a z = case (f a, f z) of
            (Just x, Just y) | x /= 0 -> Just (realToFrac y / realToFrac x - 1)
            _ -> Nothing
          split = do
            b <- base
            i <- rateInfo prices c b heldAt
            x <- case riSource i of
              SrcVia v -> Just v
              _ -> Nothing
            (d0, _) <- firstV
            (d1, _) <- lastV
            total' <- pct
            own <- change (rateOn prices c x) d0 d1
            fx <- change (rateOn prices x b) d0 d1
            pure (x, own, fx, total')
      in CommodityDetail
           { cdCommodity = c, cdSeries = ser
           , cdStart = snd <$> firstV, cdEnd = snd <$> lastV
           , cdPct = pct, cdSplit = split }
