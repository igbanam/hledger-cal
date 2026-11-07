{-# LANGUAGE OverloadedStrings #-}
module Cal.MonthTest where

import Cal.Model.Month
import Cal.Types
import Cal.Valuation (baseQty)
import Data.Time.Calendar (fromGregorian)
import Hledger hiding (Interval (..), usd)
import Support

d :: Integer -> Int -> Int -> Day
d = fromGregorian

n :: MixedAmount -> Quantity
n = baseQty "\8358"

usd :: Quantity -> MixedAmount
usd q = mixedAmount nullamt { acommodity = "$", aquantity = q }

norm :: MixedAmount -> [(CommoditySymbol, Quantity)]
norm m = [(acommodity a, aquantity a) | a <- amounts m]

cell :: MonthModel -> Day -> DayCell
cell m day = case [c | (cs, _) <- mmWeeks m, c <- cs, dcDay c == day] of
  c : _ -> c
  [] -> error "cell: day not in grid"

monthOct :: IO MonthModel
monthOct = monthModel <$> envFor ["-M", "-p", "2026-10"]

unit_rows :: Assertion
unit_rows = do
  m <- monthOct
  (length (mmWeeks m), [dcDay c | (c : _, _) <- take 1 (mmWeeks m)]) @?= (5, [d 2026 9 28])

unit_firstWeek :: Assertion
unit_firstWeek = do
  m <- monthOct
  [(n (wnNet w), n (wnIn w), n (wnOut w)) | (_, w) <- take 1 (mmWeeks m)] @?= [(2455000, 2500000, 45000)]

unit_oct30 :: Assertion
unit_oct30 = do
  m <- monthOct
  let c = cell m (d 2026 10 30)
  (n (dcNet c), fmap (\(a, b, k) -> (a, norm b, k)) (dcForeign c))
    @?= (-1860000, Just ("$", norm (usd 1200), 0))

unit_outOfMonth :: Assertion
unit_outOfMonth = do
  m <- monthOct
  dcHasPostings (cell m (d 2026 9 28)) @?= False

unit_cursor :: Assertion
unit_cursor = do
  m <- monthOct
  mmCursor m @?= d 2026 10 1

unit_cursorToday :: Assertion
unit_cursorToday = do
  m' <- monthModel <$> envFor []
  mmCursor m' @?= fixtureToday

unit_aug6Rows :: Assertion
unit_aug6Rows = do
  a <- monthModel <$> envFor ["-M", "-p", "2026-08"]
  length (mmWeeks a) @?= 6

unit_usd :: Assertion
unit_usd = do
  u <- monthModel <$> envFor ["-M", "-p", "2026-10", "-X", "$"]
  round (100 * realToFrac (baseQty "$" (dcNet (cell u (d 2026 10 2)))) :: Double) @?= (-2903 :: Int)
