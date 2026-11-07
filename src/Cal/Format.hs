-- | Number, rate, percentage and date formatting. Pure.
module Cal.Format
  ( Fmt (..), mkFmt
  , fmtSigned, fmtAbs, fmtNative, fmtQty, fmtRate, fmtPct, fmtDeltaPct
  , fmtDayMon, fmtDayLong, fmtMonthYear, fmtScale
  ) where

import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Hledger
  ( Amount (..), AmountFormat (..), AmountPrecision (..), AmountStyle (..), Journal, MixedAmount, Quantity
  , Side (..), amountStripCost, amountsRaw, defaultFmt, journalCommodityStyles, nullamt
  , showAmountB, wbToText )
import Numeric (showFFloat)

import Cal.Currency (symbolOf)
import Cal.Glyphs (Glyphs (..))
import Cal.Types (CommoditySymbol, Valuation (..))

data Fmt = Fmt
  { fStyles :: Map CommoditySymbol AmountStyle
  , fBase :: Maybe CommoditySymbol
  , fGlyphs :: Glyphs }

mkFmt :: Journal -> Valuation -> Glyphs -> Fmt
mkFmt j v g = Fmt (journalCommodityStyles j) base g
  where
    base = case v of
      Native -> Nothing
      ValuedIn c -> Just c

-- | Replace an ASCII minus with the glyph set's minus.
minus :: Fmt -> Text -> Text
minus f = T.replace "-" (gMinus (fGlyphs f))

-- | Styled, cost-free components of a mixed amount.
components :: Fmt -> MixedAmount -> [Amount]
components f = map (applyStyle . amountStripCost) . amountsRaw
  where
    applyStyle a = case M.lookup (acommodity a) (fStyles f) of
      Just st -> a { astyle = st }
      -- no journal style: symbol on the right, spaced, natural precision
      Nothing -> a { astyle = (astyle a) { ascommodityside = R, ascommodityspaced = True, asprecision = NaturalPrecision } }

render :: Fmt -> Amount -> Text
render f = minus f . renderRaw

renderRaw :: Amount -> Text
renderRaw = wbToText . showAmountB defaultFmt { displayCost = False, displayColour = False, displayZeroCommodity = True }

-- | The base amount's number only (no commodity symbol), absolute value.
renderBareAbs :: Fmt -> Amount -> Text
renderBareAbs _ a = T.strip (T.replace "-" "" (renderRaw a { acommodity = "" }))

isBase :: Fmt -> Amount -> Bool
isBase f a = Just (acommodity a) == fBase f

-- | Split into (base quantity, other components largest first).
split :: Fmt -> MixedAmount -> (Maybe Amount, [Amount])
split f m = (baseAmt, others)
  where
    cs = components f m
    baseAmt = case filter (isBase f) cs of
      [] -> Nothing
      (a : _) -> Just a
    others = sortOn (\a -> (Down (abs (aquantity a)), acommodity a))
               [a | a <- cs, not (isBase f a), aquantity a /= 0]

-- | Shared body of 'fmtSigned' and 'fmtAbs': the base part, then the others with symbols.
joined :: Bool -> Fmt -> MixedAmount -> Text
joined signed f m = case (baseAmt, others) of
  (Nothing, []) -> "0"
  (Nothing, _) -> tailText
  (Just b, _) -> T.intercalate " + " (baseText b : map (render f) others)
  where
    (baseAmt, others) = split f m
    tailText = T.intercalate " + " (map (render f) others)
    baseText b
      | aquantity b == 0 = "0"
      | otherwise = sign b <> renderBareAbs f b
    sign b
      | not signed = ""
      | aquantity b < 0 = gMinus (fGlyphs f)
      | otherwise = "+"

-- | "+2,680,000", "−2,305,000", "0"; other commodities follow, largest first.
fmtSigned :: Fmt -> MixedAmount -> Text
fmtSigned = joined True

-- | Like 'fmtSigned' without the plus sign.
fmtAbs :: Fmt -> MixedAmount -> Text
fmtAbs = joined False

-- | Every component with its symbol in its own style, joined by " + ".
fmtNative :: Fmt -> MixedAmount -> Text
fmtNative f m = case components f m of
  [] -> "0"
  cs -> T.intercalate " + " (map (render f) cs)

-- | Base-style quantity, no symbol; negative gets the glyph minus.
fmtQty :: Fmt -> Quantity -> Text
fmtQty f q = (if q < 0 then gMinus (fGlyphs f) else "") <> renderBareAbs f a
  where a = (baseAmount f) { aquantity = q }

baseAmount :: Fmt -> Amount
baseAmount f = case fBase f of
  Just c -> nullamt { acommodity = c }
  Nothing -> nullamt

-- | Grouped with "," and "."; two decimals from 1 up, four below.
fmtRate :: Quantity -> Text
fmtRate q = grouped <> "." <> frac
  where
    places = if abs q >= 1 then 2 else 4 :: Int
    scaled = round (toRational (abs q) * 10 ^ places) :: Integer
    (whole, fracN) = scaled `divMod` (10 ^ places)
    frac = T.justifyRight places '0' (T.pack (show fracN))
    grouped = (if q < 0 then "-" else "") <> group (T.pack (show whole))
    group t = T.intercalate "," (reverse (chunks (T.reverse t)))
    chunks t | T.null t = [] | otherwise = T.reverse (T.take 3 t) : chunks (T.drop 3 t)

oneDecimal :: Double -> Text
oneDecimal x = T.pack (showFFloat (Just 1) x "")

fmtPct :: Double -> Text
fmtPct x = oneDecimal (x * 100) <> "%"

fmtDeltaPct :: Fmt -> Double -> Text
fmtDeltaPct f x
  | r < 0 = gMinus (fGlyphs f) <> oneDecimal (abs r) <> "%"
  | otherwise = "+" <> oneDecimal r <> "%"
  where r = fromIntegral (round (x * 1000) :: Integer) / 10 :: Double

fmtDayMon, fmtDayLong, fmtMonthYear :: Day -> Text
fmtDayMon = T.pack . formatTime defaultTimeLocale "%-d %b"
fmtDayLong = T.pack . formatTime defaultTimeLocale "%a %-d %b %Y"
fmtMonthYear = T.pack . formatTime defaultTimeLocale "%B %Y"

-- | Base symbol plus 1-decimal M or rounded k: 2829000 gives "₦2.8M".
fmtScale :: Fmt -> Quantity -> Text
fmtScale f q = (if q < 0 then gMinus (fGlyphs f) else "") <> sym <> body
  where
    x = abs (realToFrac q) :: Double
    sym = case fBase f of
      Just c -> fromMaybe c (symbolOf c)
      Nothing -> ""
    body
      | x >= 1000000 = oneDecimal (x / 1000000) <> "M"
      | x >= 1000 = T.pack (show (round (x / 1000) :: Integer)) <> "k"
      | otherwise = T.pack (show (round x :: Integer))
