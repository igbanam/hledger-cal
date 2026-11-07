-- | The Year face model: twelve month rows grouped by quarter, with averages.
module Cal.Model.Year
  ( QuarterGroup (..)
  , YearModel (..)
  , yearModel
  ) where

import Cal.Model.Common
import Cal.Model.Shared
import Cal.Period (faceSpan, spanStatus, subSpans)
import Cal.Types
import Cal.Valuation
import Hledger hiding (Interval (..), Status)

data QuarterGroup = QuarterGroup
  { qgIndex :: !Int, qgSpan :: !Span, qgStatus :: !Status, qgNet :: !Money
  , qgSpent :: !(Maybe Double), qgMonths :: ![MonthRow] }

data YearModel = YearModel
  { ymSpan :: !Span, ymKpis :: !Kpis, ymBar :: !CurrencyBar
  , ymAvgIncome, ymAvgExpenseFull :: !(Maybe Money)
  , ymClosed :: !Int, ymOpen :: !Bool, ymOverspent :: ![Day]
  , ymGroups :: ![QuarterGroup], ymScale :: !(Maybe Quantity), ymCursor :: !Day }

yearModel :: Env -> YearModel
yearModel env = YearModel
  { ymSpan = span', ymKpis = kpis, ymBar = currencyBar env span'
  , ymAvgIncome = avg (baseQty' (kIncome kpis)) elapsed
  , ymAvgExpenseFull = avg (sum (map (baseQty' . mrExpenses) closed)) (length closed)
  , ymClosed = length closed
  , ymOpen = any ((== Open) . mrStatus) rows
  , ymOverspent = [spStart (mrSpan r) | r <- rows, mrOverspent r]
  , ymGroups = zipWith group [1 ..] (subSpans IQuarter span')
  , ymScale = scale, ymCursor = focus }
  where
    s = envSettings env
    focus = sFocus s
    today = sToday s
    span' = faceSpan FaceYear IYear focus
    kpis = kpisFor env IYear span'
    (rows, scale) = monthRows env (subSpans IMonth span')
    closed = filter ((== Closed) . mrStatus) rows
    elapsed = length closed + length (filter ((== Open) . mrStatus) rows)
    baseQty' m = maybe 0 (`baseQty` m) (envBase env)
    avg total cnt = do
      b <- envBase env
      if cnt == 0
        then Nothing
        else Just (mixedAmount nullamt { acommodity = b, aquantity = total / fromIntegral cnt })
    group i qspan =
      let k = kpisFor env IQuarter qspan
      in QuarterGroup
           { qgIndex = i, qgSpan = qspan, qgStatus = spanStatus today qspan
           , qgNet = kNet k, qgSpent = kSpent k
           , qgMonths = [r | r <- rows, spStart (mrSpan r) >= spStart qspan, spStart (mrSpan r) < spEnd qspan] }
