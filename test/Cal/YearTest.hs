{-# LANGUAGE OverloadedStrings #-}
module Cal.YearTest where

import Cal.Model.Year
import Cal.Model.Common
import Cal.Types
import Cal.Valuation (baseQty)
import Data.Time.Calendar (fromGregorian)
import Hledger hiding (Interval (..), Status)
import Support

d :: Integer -> Int -> Int -> Day
d = fromGregorian

n :: MixedAmount -> Quantity
n = baseQty "\8358"

yr :: IO YearModel
yr = yearModel <$> envFor ["-Y"]

unit_counts :: Assertion
unit_counts = do y <- yr; (ymClosed y, ymOpen y, ymOverspent y) @?= (10, True, [d 2026 8 1])

unit_income :: Assertion
unit_income = do y <- yr; n (kIncome (ymKpis y)) @?= 27680000

unit_expenses :: Assertion
unit_expenses = do y <- yr; n (kExpenses (ymKpis y)) @?= 22866000

unit_avgIncome :: Assertion
unit_avgIncome = do y <- yr; fmap (round . n) (ymAvgIncome y) @?= Just (2516364 :: Integer)

unit_avgFull :: Assertion
unit_avgFull = do y <- yr; fmap n (ymAvgExpenseFull y) @?= Just 2191600

unit_groups :: Assertion
unit_groups = do y <- yr; map qgStatus (ymGroups y) @?= [Closed, Closed, Closed, Open]

unit_pastYear :: Assertion
unit_pastYear = do
  p <- yearModel <$> envFor ["-Y", "-p", "2025"]
  (ymClosed p, ymOpen p) @?= (12, False)
