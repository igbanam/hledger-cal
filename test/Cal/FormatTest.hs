module Cal.FormatTest where

import Data.Decimal (Decimal)
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Hledger (Amount (..), MixedAmount, mixedAmount, nullamt)
import Test.Tasty.HUnit

import Cal.Format
import Cal.Glyphs (glyphsFor)
import Cal.Types (GlyphSet (..), Valuation (..))
import Support (designPath, loadFixture)

one :: Text -> Decimal -> MixedAmount
one c q = mixedAmount nullamt { acommodity = c, aquantity = q }

ngn, usd, kd, btc :: Decimal -> MixedAmount
ngn = one "₦"
usd = one "$"
kd = one "KD"
btc = one "BTC"

fmtWith :: GlyphSet -> IO Fmt
fmtWith g = (\j -> mkFmt j (ValuedIn "₦") (glyphsFor g)) <$> loadFixture designPath

fmtU :: IO Fmt
fmtU = fmtWith GlyphsUtf8

unit_signedPos, unit_signedNeg, unit_signedZero, unit_unpricedTail, unit_native :: Assertion
unit_signedPos = fmtU >>= \f -> fmtSigned f (ngn 2680000) @?= "+2,680,000"
unit_signedNeg = fmtU >>= \f -> fmtSigned f (ngn (-2305000)) @?= "−2,305,000"
unit_signedZero = fmtU >>= \f -> fmtSigned f (ngn 0) @?= "0"
unit_unpricedTail = fmtU >>= \f -> fmtSigned f (ngn 12000 <> btc 0.01) @?= "+12,000 + 0.01 BTC"
unit_native = fmtU >>= \f -> map (fmtNative f) [usd 1200, kd 5, ngn 2500000] @?= ["$1,200.00", "KD 5.000", "₦2,500,000"]

unit_abs :: Assertion
unit_abs = fmtU >>= \f -> fmtAbs f (ngn (-2305000)) @?= "2,305,000"

unit_rate, unit_pct, unit_delta, unit_dates, unit_scale, unit_asciiMinus :: Assertion
unit_rate = map fmtRate [1550, 17.5, 0.86111] @?= ["1,550.00", "17.50", "0.8611"]
unit_pct = fmtPct 0.13993 @?= "14.0%"
unit_delta = fmtU >>= \f -> (fmtDeltaPct f 0.0197, fmtDeltaPct f (-0.004)) @?= ("+2.0%", "−0.4%")
unit_dates =
  (fmtDayMon (fromGregorian 2026 10 31), fmtDayLong (fromGregorian 2026 10 30), fmtMonthYear (fromGregorian 2026 10 1))
    @?= ("31 Oct", "Fri 30 Oct 2026", "October 2026")
unit_scale = fmtU >>= \f -> (fmtScale f 2829000, fmtScale f 950000) @?= ("₦2.8M", "₦950k")
unit_asciiMinus = fmtWith GlyphsAscii >>= \f -> fmtSigned f (ngn (-5)) @?= "-5"
