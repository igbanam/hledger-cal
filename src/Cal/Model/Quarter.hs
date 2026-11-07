-- | The Quarter face model: four quarter cards over the year's month rows.
module Cal.Model.Quarter
  ( QuarterCard (..)
  , QuarterModel (..)
  , quarterModel
  ) where

import Cal.Model.Common
import Cal.Model.Shared
import Cal.Period (faceSpan, quarterOf, spanStatus, subSpans)
import Cal.Types
import Data.Time.Calendar (addDays)
import Hledger hiding (Interval (..), Status)

data QuarterCard = QuarterCard
  { qcIndex :: !Int, qcSpan :: !Span, qcStatus :: !Status, qcKpis :: !Kpis, qcMonths :: ![MonthRow] }

data QuarterModel = QuarterModel
  { qmSpan :: !Span, qmBar :: !CurrencyBar, qmCards :: ![QuarterCard], qmToDate :: !Kpis
  , qmStale :: ![CommoditySymbol], qmScale :: !(Maybe Quantity), qmCursor :: !Int }

quarterModel :: Env -> QuarterModel
quarterModel env = QuarterModel
  { qmSpan = span', qmBar = bar
  , qmCards = zipWith card [1 ..] (subSpans IQuarter span')
  , qmToDate = kpisFor env IYear toDate
  , qmStale = [c | (c, True) <- cbChoices bar]
  , qmScale = scale, qmCursor = quarterOf (sFocus s) }
  where
    s = envSettings env
    today = sToday s
    span' = faceSpan FaceQuarter IQuarter (sFocus s)
    bar = currencyBar env span'
    toDate = Span (spStart span') (min (addDays 1 today) (spEnd span'))
    (rows, scale) = monthRows env (subSpans IMonth span')
    card i qspan = QuarterCard
      { qcIndex = i, qcSpan = qspan, qcStatus = spanStatus today qspan
      , qcKpis = kpisFor env IQuarter qspan
      , qcMonths = [r | r <- rows, spStart (mrSpan r) >= spStart qspan, spStart (mrSpan r) < spEnd qspan] }
