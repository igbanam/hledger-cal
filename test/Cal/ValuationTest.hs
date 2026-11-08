{-# LANGUAGE OverloadedStrings #-}
module Cal.ValuationTest where

import Cal.Prices (mkPrices)
import Cal.Types
import Cal.Valuation
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Hledger
import Support

octSpan :: Span
octSpan = Span (fromGregorian 2026 10 1) (fromGregorian 2026 11 1)

baseSettings :: Settings
baseSettings = Settings
  { sFace = FaceMonth, sCommInterval = IMonth, sFocus = fromGregorian 2026 10 1
  , sDataRange = (fromGregorian 2026 1 1, fixtureToday)
  , sValuation = ValuedIn "₦", sRateAt = AtPeriodEnd
  , sDefaultBase = "₦", sBaseSource = BaseFromD
  , sQueryArgs = [], sQuery = Any, sFlagQuery = Any, sFlagArgs = []
  , sToday = fixtureToday, sStaleDays = 30, sForecast = False, sInferPrices = True
  , sJournalFile = designPath, sOutput = OutPrint 140, sGlyphSet = GlyphsUtf8, sColor = ColorNone }

-- | October postings of a fixture under the given settings.
octWith :: FilePath -> (Settings -> Settings) -> IO [VPosting]
octWith path f = do
  j <- loadFixture path
  let s = f baseSettings
      inOct v = vpDate v >= spStart octSpan && vpDate v < spEnd octSpan
  pure (filter inOct (valuePostings j (mkPrices (sInferPrices s) j) s octSpan (journalPostings j)))

payroll :: [VPosting] -> [VPosting]
payroll vs = [v | v <- vs, tdescription (vpTxn v) == "Household payroll — Oct"]

withDesc :: Text -> [VPosting] -> [VPosting]
withDesc d vs = [v | v <- vs, tdescription (vpTxn v) == d]

-- | Normalise a mixed amount to (commodity, quantity) pairs, ignoring styles.
norm :: MixedAmount -> [(CommoditySymbol, Quantity)]
norm m = [(acommodity a, aquantity a) | a <- amounts (mixedAmountStripCosts m)]

one :: CommoditySymbol -> Quantity -> [(CommoditySymbol, Quantity)]
one c q = [(c, q)]

approxList :: [Quantity] -> [Quantity] -> Assertion
approxList xs ys = do
  length xs @?= length ys
  mapM_ (\(x, y) -> assertBool (show x ++ " /~ " ++ show y) (abs (x - y) < 1e-6)) (zip xs ys)

unit_payrollEnd, unit_payrollThen, unit_payrollFx, unit_incomeEur, unit_rateThenMode,
  unit_nativeMode, unit_usdBase, unit_unpriced, unit_vpRate :: Assertion
unit_payrollEnd = do
  vs <- payroll <$> octWith designPath id
  map (baseQty "₦" . vpValue) vs @?= [1085000, 775000]
unit_payrollThen = do
  vs <- payroll <$> octWith designPath id
  map (baseQty "₦" . vpAtThen) vs @?= [1078000, 770000]
unit_payrollFx = do
  vs <- payroll <$> octWith designPath id
  sum (map (fxEffect "₦") vs) @?= -12000
unit_incomeEur = do
  vs <- octWith designPath id
  [(vpKind v, baseQty "₦" (vpValue v)) | v <- withDesc "Consulting" vs] @?= [(KIncome, 180000)]
unit_rateThenMode = do
  vs <- payroll <$> octWith designPath (\s -> s {sRateAt = AtTxnDate})
  map (baseQty "₦" . vpValue) vs @?= [1078000, 770000]
unit_nativeMode = do
  vs <- payroll <$> octWith designPath (\s -> s {sValuation = Native})
  -- compared by (commodity, quantity) since amount styles differ from hand-built ones
  map (norm . vpValue) vs @?= [one "$" 700, one "$" 500]
unit_usdBase = do
  vs <- octWith designPath (\s -> s {sValuation = ValuedIn "$"})
  [baseQty "$" (vpValue v) | v <- withDesc "Groceries" vs] `approxList` [45000 / 1550]
unit_unpriced = do
  vs <- octWith edgePath id
  [norm (foreignPart "₦" (vpValue v)) | v <- withDesc "Coffee beans" vs] @?= [one "BTC" 0.01]
unit_vpRate = do
  vs <- payroll <$> octWith designPath id
  map vpRate vs @?= [Just 1550, Just 1550]
