{-# LANGUAGE OverloadedStrings #-}
module Cal.WeekTest where

import Cal.Model.Common
import Cal.Model.Shared
import Cal.Model.Week
import Cal.Types
import Cal.Valuation (baseQty)
import Data.Bifunctor (second)
import Data.Time.Calendar (fromGregorian)
import Hledger hiding (Interval (..), WeekDay)
import Support

d :: Integer -> Int -> Int -> Day
d = fromGregorian

n :: MixedAmount -> Quantity
n = baseQty "\8358"

weekOf :: [String] -> Cursor -> IO WeekModel
weekOf args c = (`weekModel` c) <$> envFor args

unit_span :: Assertion
unit_span = do
  w <- weekOf ["-W", "-p", "2026-10-28"] defaultCursor
  (wkSpan w, wkIso w) @?= (Span (d 2026 10 26) (d 2026 11 2), (2026, 44))

unit_dayNets :: Assertion
unit_dayNets = do
  w <- weekOf ["-W", "-p", "2026-10-28"] defaultCursor
  map (n . wdNet) (wkDays w) @?= [0, -30500, 0, 0, -1860000, -40000, 0]

unit_top :: Assertion
unit_top = do
  w <- weekOf ["-W", "-p", "2026-10-28"] defaultCursor
  fmap (second (round . (* 100))) (wkTop w) @?= Just ("payroll", 96 :: Integer)

unit_txnFx :: Assertion
unit_txnFx = do
  w <- weekOf ["-W", "-p", "2026-10-28"] defaultCursor
  [tbFx t | t <- wdTxns (wkDays w !! 4)] @?= [Just (-12000, 1540)]

unit_activity :: Assertion
unit_activity = do
  w <- weekOf ["-W", "-p", "2026-10-28"] defaultCursor
  (aDays (kActivity (wkKpis w)), length (aCommodities (kActivity (wkKpis w)))) @?= (3, 2)

unit_cursorClamp :: Assertion
unit_cursorClamp = do
  w <- weekOf ["-W", "-p", "2026-10-30"] (Cursor 9 Nothing)
  wkCursor w @?= (d 2026 10 30, 0)

unit_w53 :: Assertion
unit_w53 = do
  e <- envFor ["-W", "-p", "2026-12-30"]
  wkIso (weekModel e defaultCursor) @?= (2026, 53)
