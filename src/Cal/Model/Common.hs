{-# LANGUAGE OverloadedStrings #-}
-- | The model environment, period postings and headline KPIs.
module Cal.Model.Common
  ( Env (..)
  , mkEnv
  , envBase
  , Money
  , Activity (..)
  , Mix (..)
  , FxCause (..)
  , Kpis (..)
  , periodPostings
  , summarize
  , kpisFor
  , group2
  ) where

import Cal.Period (inSpan, prevSpan)
import Cal.Prices (Prices, mkPrices)
import Cal.Types
import Cal.Valuation
import Data.List (nub, sortBy)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Set as S
import Hledger hiding (Interval (..))

data Env = Env { envJournal :: !Journal, envSettings :: !Settings, envPrices :: !Prices }

mkEnv :: Journal -> Settings -> Env
mkEnv j s = Env j s (mkPrices (sInferPrices s) j)

-- | The base commodity; Nothing when Native.
envBase :: Env -> Maybe CommoditySymbol
envBase e = case sValuation (envSettings e) of
  Native -> Nothing
  ValuedIn b -> Just b

type Money = MixedAmount

data Activity = Activity { aTxns :: !Int, aCommodities :: ![CommoditySymbol], aDays :: !Int }
  deriving (Eq, Show)

-- | Shares, largest first, summing to 1.
newtype Mix = Mix [(CommoditySymbol, Double)] deriving (Eq, Show)

data FxCause = FxCause { fcCommodity :: !CommoditySymbol, fcGroup :: !Text, fcAmount :: !Quantity }
  deriving (Eq, Show)

data Kpis = Kpis
  { kIncome, kExpenses, kNet :: !Money
  , kKept, kSpent :: !(Maybe Double)
  , kFx :: !(Maybe Quantity), kFxCause :: !(Maybe FxCause)
  , kIncomeMix, kExpenseMix :: !(Maybe Mix)
  , kActivity :: !Activity
  , kPrev :: !(Maybe (Money, Money)) }

-- | Query-matched postings dated in the span, valued with the span as valuation period.
periodPostings :: Env -> Span -> [VPosting]
periodPostings (Env j s prices) span' =
  filter keep (valuePostings j prices s span' (journalPostings j))
  where
    q = sQuery s
    keep v = inSpan (vpDate v) span'
      && (sForecast s || not (vpForecast v))
      && matchesPostingExtra (journalAccountType j) q (vpPosting v)

-- | Leaf of the first two components: "expenses:payroll:security" -> "payroll".
group2 :: AccountName -> Text
group2 a = case take 2 (accountNameComponents a) of
  [] -> ""
  cs -> last cs

summarize :: Env -> [VPosting] -> Kpis
summarize env vs = Kpis
  { kIncome = income, kExpenses = expenses, kNet = maum income expenses
  , kKept = ratio (\b -> baseQty b income - baseQty b expenses)
  , kSpent = ratio (\b -> baseQty b expenses)
  , kFx = fxTotal, kFxCause = cause
  , kIncomeMix = mixOf incomeVs, kExpenseMix = mixOf expenseVs
  , kActivity = activity
  , kPrev = Nothing }
  where
    base = envBase env
    incomeVs = filter ((== KIncome) . vpKind) vs
    expenseVs = filter ((== KExpense) . vpKind) vs
    amt v = case base of
      Nothing -> vpNative v
      Just _ -> vpValue v
    income = sumM (map amt incomeVs)
    expenses = sumM (map amt expenseVs)
    maum a b = plusM a (negate b)
    plusM = (+)
    sumM = sum
    ratio f = do
      b <- base
      let inc = baseQty b income
      if inc == 0 then Nothing else Just (realToFrac (f b) / realToFrac inc)
    nativeOf v = case amounts (vpNative v) of
      (a : _) -> Just (acommodity a)
      [] -> Nothing
    absBase b v = abs (baseQty b (vpValue v))
    mixOf pvs = do
      b <- base
      let m = M.fromListWith (+) [(c, absBase b v) | v <- pvs, Just c <- [nativeOf v]]
          total = sum (M.elems m)
      if total == 0
        then Nothing
        else Just (Mix (sortBy (comparing (Down . snd)) [(c, realToFrac (x / total)) | (c, x) <- M.toList m]))
    fxs b = [(c, group2 (paccount (vpPosting v)), fxEffect b v) | v <- vs, Just c <- [nativeOf v]]
    fxTotal = (\b -> sum [x | (_, _, x) <- fxs b]) <$> base
    cause = do
      b <- base
      let g = M.toList (M.fromListWith (+) [((c, grp), x) | (c, grp, x) <- fxs b])
      if maybe True (== 0) fxTotal
        then Nothing
        else case sortBy (comparing (Down . abs . snd)) g of
          [] -> Nothing
          (((c, grp), x) : _) -> Just (FxCause c grp x)
    commodities = case base of
      Nothing -> nub [c | v <- vs, Just c <- [nativeOf v]]
      Just b ->
        let m = M.fromListWith (+) [(c, absBase b v) | v <- vs, Just c <- [nativeOf v]]
            order = nub [c | v <- vs, Just c <- [nativeOf v]]
        in sortBy (comparing (Down . (m M.!))) order
    activity = Activity
      { aTxns = S.size (S.fromList [(tindex (vpTxn v)) | v <- vs])
      , aCommodities = commodities
      , aDays = S.size (S.fromList (map vpDate vs)) }

kpisFor :: Env -> Interval -> Span -> Kpis
kpisFor env i span' = (summarize env (periodPostings env span'))
  { kPrev = Just (kIncome p, kExpenses p) }
  where
    p = summarize env (periodPostings env (prevSpan i span'))
