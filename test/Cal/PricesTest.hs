{-# LANGUAGE OverloadedStrings #-}
module Cal.PricesTest where

import Cal.Prices
import Data.Time.Calendar (Day, fromGregorian)
import Support (designPath, loadFixture)
import Test.Tasty.HUnit

oct31 :: Day
oct31 = fromGregorian 2026 10 31

approx :: Maybe Double -> Double -> Assertion
approx Nothing _ = assertFailure "expected Just, got Nothing"
approx (Just x) t = assertBool ("expected " ++ show t ++ ", got " ++ show x) (abs (x - t) < 1e-9)

withP :: (Prices -> Assertion) -> Assertion
withP f = (mkPrices True <$> loadFixture designPath) >>= f

unit_usdEnd, unit_usdThen, unit_infoUsd, unit_infoEur, unit_infoVoo, unit_staleAed, unit_freshUsd, unit_reverse, unit_noInfer :: Assertion
unit_usdEnd = withP $ \p -> rateOn p "$" "₦" oct31 @?= Just 1550
unit_usdThen = withP $ \p -> rateOn p "$" "₦" (fromGregorian 2026 10 30) @?= Just 1540
unit_infoUsd = withP $ \p -> rateInfo p "$" "₦" oct31 @?= Just (RateInfo 1550 SrcInferred oct31)
unit_infoEur = withP $ \p -> rateInfo p "€" "₦" oct31 @?= Just (RateInfo 1800 SrcDeclared (fromGregorian 2026 10 20))
unit_infoVoo = withP $ \p -> rateInfo p "VOO" "₦" oct31 @?= Just (RateInfo 868000 (SrcVia "$") oct31)
unit_staleAed = withP $ \p -> (staleAge 30 oct31 =<< rateInfo p "Dh" "₦" oct31) @?= Just 78
unit_freshUsd = withP $ \p -> (staleAge 30 oct31 =<< rateInfo p "$" "₦" oct31) @?= Nothing
unit_reverse = withP $ \p -> fmap (realToFrac . riRate) (rateInfo p "₦" "$" oct31) `approx` (1 / 1550)
unit_noInfer = do
  p <- mkPrices False <$> loadFixture designPath
  rateOn p "$" "₦" oct31 @?= Nothing
