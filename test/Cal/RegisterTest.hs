{-# LANGUAGE OverloadedStrings #-}
module Cal.RegisterTest where

import Cal.Model.Common (Activity (..), Env, Kpis (..), kpisFor)
import Cal.Model.Register
import Cal.Model.Shared (DayDetail (..), TxnBlock (..), dayDetail)
import Cal.Types
import Cal.Valuation (baseQty)
import Data.Time.Calendar (fromGregorian)
import Hledger hiding (Interval (..), Status)
import Support

d :: Integer -> Int -> Int -> Day
d = fromGregorian

octSpan :: Span
octSpan = Span (d 2026 10 1) (d 2026 11 1)

n :: MixedAmount -> Quantity
n = baseQty "₦"

unit_oct30 :: Assertion
unit_oct30 = do
  e <- envFor ["-M", "-p", "2026-10"]
  let rs = registerRows e octSpan (d 2026 10 30)
  ( map rgDate rs, map rgDescription rs, map (n . rgRunning) rs )
    @?= ( [Just (d 2026 10 30), Nothing], [Just "Household payroll — Oct", Nothing], [-1085000, -1860000] )

unit_empty :: Assertion
unit_empty = do
  e <- envFor ["-M", "-p", "2026-10"]
  map rgDate (registerRows e octSpan (d 2026 10 3)) @?= []

-- | Two files, each with a transaction #1: identity must survive the merge.
multiEnv :: IO Env
multiEnv = envFor ["-M", "-p", "2026-10", "-f", "test/fixtures/multi-a.journal", "-f", "test/fixtures/multi-b.journal"]

unit_multiFileActivity :: Assertion
unit_multiFileActivity = do
  e <- multiEnv
  aTxns (kActivity (kpisFor e IMonth octSpan)) @?= 3

unit_multiFileTxnBlocks :: Assertion
unit_multiFileTxnBlocks = do
  e <- multiEnv
  let dd = dayDetail e octSpan (d 2026 10 3)
  (map tbDescription (ddTxns dd), map (length . tbRows) (ddTxns dd)) @?= (["Lunch", "Lunch"], [1, 1])

unit_multiFileRegister :: Assertion
unit_multiFileRegister = do
  e <- multiEnv
  map rgDescription (registerRows e octSpan (d 2026 10 3)) @?= [Just "Lunch", Just "Lunch"]

-- | The second file declares the ₦ style too, so "₦2,000" is two thousand.
unit_multiFileAmounts :: Assertion
unit_multiFileAmounts = do
  e <- multiEnv
  map (n . rgRunning) (registerRows e octSpan (d 2026 10 3)) @?= [-1000, -3000]
  baseQty "₦" (kExpenses (kpisFor e IMonth octSpan)) @?= 3500
