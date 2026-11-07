{-# LANGUAGE OverloadedStrings #-}
module Cal.QuarterTest where

import Cal.Model.Common
import Cal.Model.Quarter
import Cal.Model.Shared
import Cal.Types
import Cal.Valuation (baseQty)
import Data.Time.Calendar (fromGregorian)
import Hledger hiding (Interval (..), Status)
import Support

d :: Integer -> Int -> Int -> Day
d = fromGregorian

n :: MixedAmount -> Quantity
n = baseQty "\8358"

qm :: IO QuarterModel
qm = quarterModel <$> envFor ["-Q"]

unit_cards :: Assertion
unit_cards = do
  q <- qm
  (length (qmCards q), map qcStatus (qmCards q), qmCursor q) @?= (4, [Closed, Closed, Closed, Open], 4)

unit_toDate :: Assertion
unit_toDate = do q <- qm; n (kIncome (qmToDate q)) @?= 27680000

unit_stale :: Assertion
unit_stale = do q <- qm; qmStale q @?= ["Dh"]

unit_q4Months :: Assertion
unit_q4Months = do
  q <- qm
  map (spStart . mrSpan) (qcMonths (qmCards q !! 3)) @?= [d 2026 10 1, d 2026 11 1, d 2026 12 1]

unit_q4Exp :: Assertion
unit_q4Exp = do q <- qm; n (kExpenses (qcKpis (qmCards q !! 3))) @?= 2715000

unit_staleAges :: Assertion
unit_staleAges = do q <- qm; qmStaleAges q @?= [("Dh", 84)]
