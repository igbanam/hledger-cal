-- | The Week face model: seven day columns, KPIs and the top expense group.
module Cal.Model.Week
  ( WeekDay (..)
  , WeekModel (..)
  , weekModel
  ) where

import Cal.Model.Common
import Cal.Model.Shared
import Cal.Period (faceSpan, isoWeek)
import Cal.Types
import Cal.Valuation
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Data.Time.Calendar (addDays)
import Hledger hiding (Interval (..), Span (..), WeekDay)

data WeekDay = WeekDay
  { wdDay :: !Day, wdNet :: !Money, wdSpendFrac :: !(Maybe Double), wdTxns :: ![TxnBlock] }

data WeekModel = WeekModel
  { wkSpan :: !Span, wkIso :: !(Integer, Int), wkKpis :: !Kpis, wkBar :: !CurrencyBar
  , wkTop :: !(Maybe (Text, Double)), wkDays :: ![WeekDay], wkCursor :: !(Day, Int) }

weekModel :: Env -> Cursor -> WeekModel
weekModel env cur = WeekModel
  { wkSpan = span', wkIso = isoWeek focus, wkKpis = kpisFor env IWeek span'
  , wkBar = currencyBar env span'
  , wkTop = top, wkDays = days
  , wkCursor = (focus, cursorTxn) }
  where
    focus = sFocus (envSettings env)
    span' = faceSpan FaceWeek IWeek focus
    vs = periodPostings env span'
    start = spStart span'
    dayList = [addDays k start | k <- [0 .. 6]]
    amt v = case envBase env of
      Nothing -> vpNative v
      Just _ -> vpValue v
    expenses = filter ((== KExpense) . vpKind)
    dayExpenseBase day = case envBase env of
      Just b -> baseQty b (maSum [amt v | v <- expenses (onDay day)])
      Nothing -> 0
    maxExp = maximum (0 : map dayExpenseBase dayList)
    onDay day = filter ((== day) . vpDate) vs
    days = map mk dayList
    mk day =
      let ps = onDay day
          inc = maSum [amt v | v <- ps, vpKind v == KIncome]
          out = maSum [amt v | v <- ps, vpKind v == KExpense]
          frac = case envBase env of
            Just _ | maxExp > 0 -> Just (realToFrac (dayExpenseBase day / maxExp))
            _ -> Nothing
      in WeekDay day (inc - out) frac (txnBlocks env ps)
    top = do
      b <- envBase env
      let es = expenses vs
          total = baseQty b (maSum (map vpValue es))
          groups = M.toList (M.fromListWith (+)
            [(group2 (paccount (vpPosting v)), baseQty b (vpValue v)) | v <- es])
      if total == 0
        then Nothing
        else case sortBy (comparing (Down . snd)) groups of
          (g, x) : _ -> Just (g, realToFrac (x / total))
          [] -> Nothing
    cursorTxn =
      let cnt = length (concat [wdTxns w | w <- days, wdDay w == focus])
      in if cnt == 0 then 0 else max 0 (min (cnt - 1) (cuWeekTxn cur))
