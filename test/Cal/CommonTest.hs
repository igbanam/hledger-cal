{-# LANGUAGE OverloadedStrings #-}
module Cal.CommonTest where

import Cal.Model.Common
import Cal.Types
import Cal.Valuation
import Data.Bifunctor (bimap)
import Data.Time.Calendar (fromGregorian)
import Hledger hiding (Interval (..))
import Support

d :: Integer -> Int -> Int -> Day
d = fromGregorian

n :: MixedAmount -> Quantity
n = baseQty "₦"

octSpan :: Span
octSpan = Span (d 2026 10 1) (d 2026 11 1)

btc :: Quantity -> [(CommoditySymbol, Quantity)]
btc q = [("BTC", q)]

norm :: MixedAmount -> [(CommoditySymbol, Quantity)]
norm m = [(acommodity a, aquantity a) | a <- amounts m]

octK :: IO Kpis
octK = do
  oct <- envFor ["-M", "-p", "2026-10"]
  pure (kpisFor oct IMonth octSpan)

unit_octTotals :: IO ()
unit_octTotals = do k <- octK; (n (kIncome k), n (kExpenses k), n (kNet k)) @?= (2680000, 2305000, 375000)
unit_octKept :: IO ()
unit_octKept = do k <- octK; fmap (round . (* 1000)) (kKept k) @?= Just (140 :: Int)
unit_octFx :: IO ()
unit_octFx = do k <- octK; (kFx k, kFxCause k) @?= (Just (-12000), Just (FxCause "$" "payroll" (-12000)))
unit_octActivity :: IO ()
unit_octActivity = do k <- octK; kActivity k @?= Activity 11 ["₦", "$", "€", "₹", "KD"] 11
unit_octPrev :: IO ()
unit_octPrev = do k <- octK; fmap (bimap n n) (kPrev k) @?= Just (2500000, 2124000)
unit_octMixIn :: IO ()
unit_octMixIn = do
  k <- octK
  fmap (\(Mix xs) -> [(c, round (s * 1000) :: Int) | (c, s) <- xs]) (kIncomeMix k) @?= Just [("₦", 933), ("€", 67)]
unit_octMixOut :: IO ()
unit_octMixOut = do k <- octK; fmap (\(Mix xs) -> map fst xs) (kExpenseMix k) @?= Just ["$", "₦", "₹", "KD"]
unit_week44 :: IO ()
unit_week44 = do
  e <- envFor ["-W", "-p", "2026-10-28"]
  let w = kpisFor e IWeek (Span (d 2026 10 26) (d 2026 11 2))
  (n (kIncome w), n (kExpenses w)) @?= (0, 1930500)
unit_native :: IO ()
unit_native = do
  e <- envFor ["-M", "-p", "2026-10", "--native"]
  let kn = kpisFor e IMonth octSpan
  (kKept kn, kFx kn, kIncomeMix kn) @?= (Nothing, Nothing, Nothing)
unit_emptyDec :: IO ()
unit_emptyDec = do
  e <- envFor ["-M", "-p", "2026-12"]
  let kd = kpisFor e IMonth (Span (d 2026 12 1) (d 2027 1 1))
  (n (kIncome kd), kKept kd, kFx kd, aTxns (kActivity kd)) @?= (0, Nothing, Just 0, 0)
unit_unpriced :: IO ()
unit_unpriced = do
  e <- envFor ["-M", "-p", "2026-10", "-f", edgePath]
  let ke = kpisFor e IMonth octSpan
  norm (foreignPart "₦" (kExpenses ke)) @?= btc 0.01
unit_query :: IO ()
unit_query = do
  e <- envFor ["-M", "-p", "2026-10", "acct:payroll"]
  n (kExpenses (kpisFor e IMonth octSpan)) @?= 1860000

-- | With an explicit -p, the previous period still gets its forecast postings.
unit_forecastPrevWithPeriod :: Assertion
unit_forecastPrevWithPeriod = do
  e <- envFor ["-M", "-p", "2026-12", "--forecast", "-f", "test/fixtures/forecast.journal"]
  let k = kpisFor e IMonth (Span (d 2026 12 1) (d 2027 1 1))
  (n (kExpenses k), fmap (bimap n n) (kPrev k)) @?= (4000, Just (0, 5000))

unit_forecastNextWithPeriod :: Assertion
unit_forecastNextWithPeriod = do
  e <- envFor ["-M", "-p", "2026-11", "--forecast", "-f", "test/fixtures/forecast.journal"]
  n (kExpenses (kpisFor e IMonth (Span (d 2026 12 1) (d 2027 1 1)))) @?= 4000

-- | An unpriced commodity has no share of the valued total: no "BTC 0.0%".
unit_mixDropsZeroShares :: Assertion
unit_mixDropsZeroShares = do
  e <- envFor ["-M", "-p", "2026-10", "-f", edgePath]
  let k = kpisFor e IMonth octSpan
      zeros = [ c | Just (Mix xs) <- [kIncomeMix k, kExpenseMix k], (c, 0) <- xs ]
  zeros @?= []
