-- | The Month face model: calendar cells, week nets, KPIs and day detail.
module Cal.Model.Month
  ( DayCell (..)
  , WeekNet (..)
  , MonthModel (..)
  , monthModel
  ) where

import Cal.Model.Common
import Cal.Model.Shared
import Cal.Period (faceSpan, inSpan, monthGrid)
import Cal.Types
import Cal.Valuation
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..), comparing)
import Hledger hiding (Interval (..), Span (..))

-- | dcForeign: the largest non-base commodity that day by |value|, its
-- native sum, and the number of other non-base commodities.
data DayCell = DayCell
  { dcDay :: !Day, dcInMonth :: !Bool, dcNet :: !Money, dcHasPostings :: !Bool
  , dcForeign :: !(Maybe (CommoditySymbol, MixedAmount, Int)) }

data WeekNet = WeekNet { wnNet, wnIn, wnOut :: !Money }

data MonthModel = MonthModel
  { mmSpan :: !Span, mmKpis :: !Kpis, mmBar :: !CurrencyBar
  , mmWeeks :: ![([DayCell], WeekNet)], mmDetail :: !DayDetail
  , mmRates :: !(Maybe RatesPanel), mmCursor :: !Day }

monthModel :: Env -> MonthModel
monthModel env = MonthModel
  { mmSpan = span', mmKpis = kpisFor env IMonth span'
  , mmBar = currencyBar env span'
  , mmWeeks = map week (monthGrid focus)
  , mmDetail = dayDetail env span' focus
  , mmRates = ratesPanel env IMonth span' vs
  , mmCursor = focus }
  where
    s = envSettings env
    focus = sFocus s
    span' = faceSpan FaceMonth IMonth focus
    vs = periodPostings env span'
    base = case envBase env of
      Just b -> b
      Nothing -> sDefaultBase s
    amt v = case envBase env of
      Nothing -> vpNative v
      Just _ -> vpValue v
    totals ps =
      ( maSum [amt v | v <- ps, vpKind v == KIncome]
      , maSum [amt v | v <- ps, vpKind v == KExpense] )
    week days =
      let cells = map cell days
          inM = [v | c <- cells, dcInMonth c, v <- onDay (dcDay c)]
          (i, o) = totals inM
      in (cells, WeekNet (i - o) i o)
    onDay day = filter ((== day) . vpDate) vs
    cell day
      | not (inSpan day span') = DayCell day False mempty False Nothing
      | otherwise =
          let ps = onDay day
              (i, o) = totals ps
          in DayCell day True (i - o) (not (null ps)) (foreignOf ps)
    nativeC v = case amounts (vpNative v) of
      (a : _) -> Just (acommodity a)
      [] -> Nothing
    foreignOf ps =
      let foreigns = [(c, v) | v <- ps, Just c <- [nativeC v], c /= base]
          weight (c, v) = case envBase env of
            Just b -> (c, abs (baseQty b (vpValue v)))
            Nothing -> (c, abs (sum [aquantity a | a <- amounts (vpNative v)]))
          totalsBy = M.toList (M.fromListWith (+) (map weight foreigns))
      in case sortBy (comparing (Down . snd)) totalsBy of
           [] -> Nothing
           (c, _) : rest ->
             Just (c, maSum [vpNative v | (c', v) <- foreigns, c' == c], length rest)
