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
