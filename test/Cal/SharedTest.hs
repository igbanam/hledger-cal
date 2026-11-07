{-# LANGUAGE OverloadedStrings #-}
module Cal.SharedTest where

import Cal.Model.Common
import Cal.Model.Shared
import Cal.Period (subSpans)
import Cal.Prices (RateInfo (..))
import Cal.Types
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Hledger hiding (Interval (..), Status, usd)
import Support

d :: Integer -> Int -> Int -> Day
d = fromGregorian

octSpan, year2026 :: Span
octSpan = Span (d 2026 10 1) (d 2026 11 1)
year2026 = Span (d 2026 1 1) (d 2027 1 1)

one :: CommoditySymbol -> Quantity -> MixedAmount
one c q = mixedAmount nullamt { acommodity = c, aquantity = q }

ngn, usd :: Quantity -> MixedAmount
ngn = one "₦"
usd = one "$"

norm :: MixedAmount -> [(CommoditySymbol, Quantity)]
norm m = [(acommodity a, aquantity a) | a <- amounts m]

-- | MixedAmount-free projections for structural comparison.
hintN :: ForecastHint -> (Text, [(CommoditySymbol, Quantity)], Maybe Quantity, Int)
hintN h = (fhDescription h, norm (fhNative h), fhValued h, fhMore h)

data NoteN = NFx Quantity [(CommoditySymbol, Quantity)] Quantity | NNoConv CommoditySymbol | NNone
  deriving (Eq, Show)

noteN :: DetailNote -> NoteN
noteN (NoteFx r v x) = NFx r (norm v) x
noteN (NoteNoConversion c) = NNoConv c
noteN NoteNone = NNone

yearRows :: [String] -> IO ([MonthRow], Maybe Quantity)
yearRows args = do
  y <- envFor args
  pure (monthRows y (subSpans IMonth year2026))

rows2026 :: IO ([MonthRow], Maybe Quantity)
rows2026 = yearRows ["-Y", "-p", "2026"]

octEnv :: IO Env
octEnv = envFor ["-M", "-p", "2026-10"]

unit_overspent :: IO ()
unit_overspent = do
  (rows, _) <- rows2026
  [spStart (mrSpan r) | r <- rows, mrOverspent r] @?= [d 2026 8 1]

unit_scale :: IO ()
unit_scale = do (_, scale) <- rows2026; scale @?= Just 2829000

unit_statuses :: IO ()
unit_statuses = do (rows, _) <- rows2026; map mrStatus (drop 9 rows) @?= [Closed, Open, Future]

unit_octComms :: IO ()
unit_octComms = do (rows, _) <- rows2026; mrCommodities (rows !! 9) @?= ["₦", "$", "€", "₹", "KD"]

unit_decHint :: IO ()
unit_decHint = do
  (rows, _) <- rows2026
  fmap hintN (mrHint (rows !! 11)) @?= Just (hintN (ForecastHint "Household payroll" (usd 1200) (Just 1860000) 0))

unit_noHintWithForecast :: IO ()
unit_noHintWithForecast = do
  (rows, _) <- yearRows ["-Y", "-p", "2026", "--forecast"]
  fmap hintN (mrHint (rows !! 11)) @?= Nothing

unit_rates :: IO ()
unit_rates = do
  oct <- octEnv
  fmap (map (\r -> (rtCommodity r, fmap riRate (rtInfo r), fmap (round . (* 1000)) (rtDeltaPct r), rtStaleDays r)) . rpRows)
    (ratesPanel oct IMonth octSpan (periodPostings oct octSpan))
    @?= Just
      [ ("$", Just 1550, Just (20 :: Int), Nothing), ("Dh", Just 422, Nothing, Just 78), ("€", Just 1800, Just 7, Nothing)
      , ("₹", Just 17.5, Just 4, Nothing), ("KD", Just 5000, Just 2, Nothing) ]

unit_bar :: IO ()
unit_bar = do
  oct <- octEnv
  let b = currencyBar oct octSpan
  (cbChoices b, cbPriceNote b)
    @?= ([("₦", False), ("$", False), ("Dh", True), ("€", False), ("₹", False), ("KD", False)], Just "market · inferred")

unit_day30 :: IO ()
unit_day30 = do
  oct <- octEnv
  let dd = dayDetail oct octSpan (d 2026 10 30)
  (map tbDescription (ddTxns dd), ddPostings dd, ddCommodities dd, noteN (ddNote dd))
    @?= (["Household payroll — Oct"], 2, ["$"], noteN (NoteFx 1540 (ngn 1848000) (-12000)))

unit_day30Usd :: IO ()
unit_day30Usd = do
  u <- envFor ["-M", "-p", "2026-10", "-X", "$"]
  noteN (ddNote (dayDetail u octSpan (d 2026 10 30))) @?= NNoConv "$"

unit_location :: IO ()
unit_location = do
  oct <- octEnv
  [drLocation r | t <- ddTxns (dayDetail oct octSpan (d 2026 10 27)), r <- tbRows t] @?= [Just "Port Harcourt"]
