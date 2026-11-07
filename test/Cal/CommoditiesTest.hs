{-# LANGUAGE OverloadedStrings #-}
module Cal.CommoditiesTest where

import Cal.Model.Commodities
import Cal.Prices (RateInfo (..), RateSource (..))
import Cal.Types
import Data.List (find)
import Data.Maybe (isJust)
import Test.Tasty.HUnit
import Support

cm :: IO CommoditiesModel
cm = (`commoditiesModel` defaultCursor) <$> envFor ["--commodities", "-p", "2026-10"]

row :: CommoditySymbol -> CommoditiesModel -> Maybe CommodityRow
row s c = find ((== s) . crCommodity) (cmCurrencies c ++ cmSecurities c)

unit_order :: Assertion
unit_order = do
  c <- cm
  (map crCommodity (cmCurrencies c), map crCommodity (cmSecurities c))
    @?= (["\8358", "$", "Dh", "\8364", "\8377", "KD"], ["VOO", "MTNN"])

unit_mtnn :: Assertion
unit_mtnn = do
  c <- cm
  fmap (\r -> (crHeld r, crChange r, riRate <$> crPrice r, crValue r)) (row "MTNN" c)
    @?= Just (1500, 500, Just 420, Just 630000)

unit_voo :: Assertion
unit_voo = do
  c <- cm
  fmap (\r -> (crValue r, riSource <$> crPrice r)) (row "VOO" c) @?= Just (Just 2604000, Just (SrcVia "$"))

unit_usdHeld :: Assertion
unit_usdHeld = do c <- cm; fmap crHeld (row "$" c) @?= Just 2500

unit_gainMtnn :: Assertion
unit_gainMtnn = do c <- cm; fmap crGain (row "MTNN" c) @?= Just (Just 125000)

unit_gainVoo :: Assertion
unit_gainVoo = do c <- cm; fmap crGain (row "VOO" c) @?= Just (Just 516000)

unit_stale :: Assertion
unit_stale = do
  c <- cm
  (fmap crStale (row "Dh" c), cmStaleCount c, cmUnpriced c) @?= (Just (Just 78), 1, 0)

unit_spark :: Assertion
unit_spark = do c <- cm; fmap (take 3 . crSpark) (row "MTNN" c) @?= Just [Nothing, Nothing, Just 300]

unit_vooSplit :: Assertion
unit_vooSplit = do
  c' <- (`commoditiesModel` Cursor 0 6) <$> envFor ["--commodities", "-p", "2026-10"]
  fmap (round . (* 1000)) (cmDetail c' >>= cdPct) @?= Just (222 :: Int)

unit_unpriced :: Assertion
unit_unpriced = do
  e <- (`commoditiesModel` defaultCursor) <$> envFor ["--commodities", "-p", "2026-10", "-f", edgePath]
  cmUnpriced e @?= 1

unit_deltaMtnn :: Assertion
unit_deltaMtnn = do
  c <- cm
  fmap (fmap (round . (* 1000)) . crDeltaPct) (row "MTNN" c) @?= Just (Just (37 :: Int))

unit_deltaStale :: Assertion
unit_deltaStale = do
  c <- cm
  (fmap crDeltaPct (row "Dh" c), fmap (isJust . crDeltaPct) (row "\8364" c)) @?= (Just Nothing, Just True)
