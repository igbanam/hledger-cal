{-# LANGUAGE OverloadedStrings #-}
module Cal.RegisterTest where

import Cal.Model.Register
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
