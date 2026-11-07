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

-- | Cost-free components, summed per commodity, zeros dropped, in journal style.
components :: Fmt -> MixedAmount -> [Amount]
components f m =
  [ applyStyle c q | (c, q) <- M.toList (M.fromListWith (+) [(acommodity a, aquantity a) | a <- map amountStripCost (amountsRaw m)])
  , q /= 0 ]
  where
    applyStyle c q = case M.lookup c (fStyles f) of
      Just st -> nullamt { acommodity = c, aquantity = q, astyle = st }
      -- no journal style: symbol on the right, spaced, natural precision
      Nothing -> let a0 = nullamt { acommodity = c, aquantity = q }
                 in a0 { astyle = (astyle a0) { ascommodityside = R, ascommodityspaced = True, asprecision = NaturalPrecision } }

renderRaw :: Amount -> Text
renderRaw = wbToText . showAmountB defaultFmt { displayCost = False, displayColour = False, displayZeroCommodity = True }

-- | Absolute value with its symbol, no sign.
renderAbs :: Amount -> Text
renderAbs a = renderRaw a { aquantity = abs (aquantity a) }

-- | Absolute value without the commodity symbol.
renderBareAbs :: Amount -> Text
renderBareAbs a = T.strip (renderRaw a { acommodity = "", aquantity = abs (aquantity a) })

isBase :: Fmt -> Amount -> Bool
isBase f a = Just (acommodity a) == fBase f

-- | Components ordered base first (as is), then others by absolute quantity descending.
ordered :: Fmt -> MixedAmount -> [Amount]
ordered f m = filter (isBase f) cs ++ sortOn (\a -> (Down (abs (aquantity a)), acommodity a)) (filter (not . isBase f) cs)
  where cs = components f m

-- | Shared body of 'fmtSigned' and 'fmtAbs'. Each piece carries its sign; the base piece has no symbol.
joined :: Bool -> Fmt -> MixedAmount -> Text
joined signed f m = case ordered f m of
  [] -> "0"
  (x : xs) -> piece True x <> T.concat (map (piece False) xs)
  where
    neg a = aquantity a < 0
    body a = if isBase f a then renderBareAbs a else renderAbs a
    piece first a
      | not signed = (if first then "" else " + ") <> body a
      | first = (if neg a then gMinus (fGlyphs f) else "+") <> body a
      | otherwise = (if neg a then " " <> gMinus (fGlyphs f) <> " " else " + ") <> body a

-- | "+2,680,000", "−2,305,000", "0"; other commodities follow, largest first,
-- each joined by " + " or " − " according to its sign.
fmtSigned :: Fmt -> MixedAmount -> Text
fmtSigned = joined True

-- | Like 'fmtSigned' without any signs.
fmtAbs :: Fmt -> MixedAmount -> Text
fmtAbs = joined False

-- | Every component with its symbol in its own style, joined by " + ".
fmtNative :: Fmt -> MixedAmount -> Text
fmtNative f m = case components f m of
  [] -> "0"
  cs -> T.intercalate " + " (map one cs)
  where one a = (if aquantity a < 0 then gMinus (fGlyphs f) else "") <> renderAbs a

-- | Base-style quantity, no symbol: "+12,000", "0", "−12,000".
fmtQty :: Fmt -> Quantity -> Text
fmtQty f q
  | q == 0 = "0"
  | otherwise = (if q < 0 then gMinus (fGlyphs f) else "+") <> renderBareAbs a
  where a = (baseAmount f) { aquantity = q }

baseAmount :: Fmt -> Amount
baseAmount f = styled (case fBase f of { Just c -> nullamt { acommodity = c }; Nothing -> nullamt })
  where styled a = maybe a (\st -> a { astyle = st }) (M.lookup (acommodity a) (fStyles f))

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
