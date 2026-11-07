-- | Currency vs. security classification of commodity symbols.
module Cal.Currency
  ( symbolCodes
  , isCurrency
  , currencyCode
  , codeOf
  , symbolOf
  , chipLabel
  ) where

import Cal.Currency.Table (iso4217)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Hledger (CommoditySymbol)

-- | Currency symbols and the ISO 4217 code each stands for.
symbolCodes :: [(Text, Text)]
symbolCodes =
  [ ("$", "USD"), ("€", "EUR"), ("£", "GBP"), ("¥", "JPY"), ("₦", "NGN")
  , ("₹", "INR"), ("₩", "KRW"), ("₽", "RUB"), ("₺", "TRY"), ("₱", "PHP")
  , ("₫", "VND"), ("₴", "UAH"), ("₵", "GHS"), ("₪", "ILS"), ("฿", "THB")
  , ("R$", "BRL"), ("Dh", "AED"), ("KD", "KWD"), ("KSh", "KES")
  , ("CHF", "CHF"), ("R", "ZAR")
  ]

isCurrency :: CommoditySymbol -> Bool
isCurrency c = c `Set.member` iso4217 || c `elem` map fst symbolCodes

currencyCode :: CommoditySymbol -> Maybe Text
currencyCode c
  | c `Set.member` iso4217 = Just c
  | otherwise = lookup c symbolCodes

codeOf :: CommoditySymbol -> Text
codeOf c = fromMaybe c (currencyCode c)

-- | The symbol for a commodity: itself if it is one, else the first symbol for its code.
symbolOf :: CommoditySymbol -> Maybe Text
symbolOf c
  | c `elem` map fst symbolCodes = Just c
  | otherwise = fst <$> (currencyCode c >>= \code -> firstFor code)
  where
    firstFor code = case [p | p@(_, k) <- symbolCodes, k == code] of
      (p : _) -> Just p
      [] -> Nothing

-- | "<symbol> <CODE>" when both are known and differ, else the commodity.
chipLabel :: CommoditySymbol -> Text
chipLabel c = case (symbolOf c, currencyCode c) of
  (Just s, Just code) | s /= code -> s <> " " <> code
  _ -> c
