module Cal.CurrencyTest where

import Cal.Currency
import Test.Tasty.HUnit

unit_currencies :: Assertion
unit_currencies = map isCurrency ["₦", "USD", "KD", "XAF", "MTNN", "VOO", "XAU", "BTC"] @?= [True, True, True, True, False, False, False, False]

unit_codes :: Assertion
unit_codes = map codeOf ["₦", "$", "Dh", "KD", "MTNN"] @?= ["NGN", "USD", "AED", "KWD", "MTNN"]

unit_chips :: Assertion
unit_chips = map chipLabel ["₦", "$", "USD", "MTNN"] @?= ["₦ NGN", "$ USD", "$ USD", "MTNN"]

unit_chipNoDup :: Assertion
unit_chipNoDup = chipLabel "CHF" @?= "CHF"

-- | ISO codes match regardless of case, so a journal written in lowercase
-- (ngn, eur) gets currencies, not securities; tickers stay securities.
unit_lowercaseCodes :: Assertion
unit_lowercaseCodes = do
  map isCurrency ["eur", "aed", "Usd", "btc", "voo"] @?= [True, True, True, False, False]
  map codeOf ["eur", "aed"] @?= ["EUR", "AED"]
  chipLabel "eur" @?= "€ EUR"
