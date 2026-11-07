-- | Postings carrying native and valued amounts, plus base-commodity helpers.
module Cal.Valuation
  ( VPosting (..)
  , valuePostings
  , baseQty
  , foreignPart
  , fxEffect
  ) where

import Cal.Journal (isForecastTxn, postingKind)
import Cal.Period (lastDay)
import Cal.Prices (Prices (..), rateOn)
import Cal.Types
import Data.Maybe (mapMaybe)
import Hledger

data VPosting = VPosting
  { vpPosting :: !Posting
  , vpTxn :: !Transaction
  , vpDate :: !Day
  , vpKind :: !Kind
  , vpNative :: !MixedAmount
  , vpValue :: !MixedAmount
  , vpAtEnd, vpAtThen :: !MixedAmount
  , vpRate :: !(Maybe Quantity)
  , vpForecast :: !Bool
  }

-- | Keeps postings with a Kind and a transaction; no date or query filtering.
valuePostings :: Journal -> Prices -> Settings -> Span -> [Posting] -> [VPosting]
valuePostings j prices s span' = mapMaybe go
  where
    go p = do
      t <- ptransaction p
      k <- postingKind j p
      let date = postingDate p
          stripped = mixedAmountStripCosts (pamount p)
          native = if k == KIncome then negate stripped else stripped
          valueAt d = case sValuation s of
            Native -> native
            ValuedIn base -> mixedAmountValueAtDate (prOracle prices) (prStyles prices) (Just base) d native
          atEnd = valueAt (lastDay span')
          atThen = valueAt date
          applied = case sRateAt s of
            AtPeriodEnd -> (atEnd, lastDay span')
            AtTxnDate -> (atThen, date)
            AtToday -> (valueAt (sToday s), sToday s)
          rate = case (sValuation s, amounts native) of
            (ValuedIn base, [a])
              | acommodity a /= base -> rateOn prices (acommodity a) base (snd applied)
            _ -> Nothing
      pure VPosting
        { vpPosting = p, vpTxn = t, vpDate = date, vpKind = k
        , vpNative = native, vpValue = fst applied
        , vpAtEnd = atEnd, vpAtThen = atThen
        , vpRate = rate, vpForecast = isForecastTxn t }

-- | The base-commodity component (0 if absent).
baseQty :: CommoditySymbol -> MixedAmount -> Quantity
baseQty base m = sum [aquantity a | a <- amounts m, acommodity a == base]

-- | Every non-base component.
foreignPart :: CommoditySymbol -> MixedAmount -> MixedAmount
foreignPart base m = mixed [a | a <- amounts m, acommodity a /= base]

-- | Income: base(atEnd) - base(atThen); expense: the negation.
fxEffect :: CommoditySymbol -> VPosting -> Quantity
fxEffect base v = sign (baseQty base (vpAtEnd v) - baseQty base (vpAtThen v))
  where sign = if vpKind v == KIncome then id else negate
