module Cal.GlyphsTest where

import qualified Data.Text as T
import Test.Tasty.HUnit

import Cal.Glyphs
import Cal.Types (GlyphSet (..))

u, a :: Glyphs
u = glyphsFor GlyphsUtf8
a = glyphsFor GlyphsAscii

unit_hbarFull, unit_hbarEighth, unit_hbarAscii, unit_hbarClamp :: Assertion
unit_hbarFull = hbar u 10 1.0 @?= T.replicate 10 "█"
unit_hbarEighth = hbar u 2 0.5625 @?= "█▏"
unit_hbarAscii = hbar a 4 0.6 @?= "##"
unit_hbarClamp = hbar u 3 1.7 @?= "███"

unit_spark, unit_sparkAscii, unit_sparkFlat :: Assertion
unit_spark = sparkline u [Nothing, Just 1, Just 8, Just 4.5] @?= "·▁█▅"
unit_sparkAscii = sparkline a [Just 1, Just 8] @?= "_^"
unit_sparkFlat = sparkline u [Just 5, Just 5] @?= "▄▄"

unit_chartHeight, unit_chartAscii, unit_chartWidths, unit_chartEmpty :: Assertion
unit_chartHeight = length (lineChart u 12 2 (map Just [1 .. 12])) @?= 2
unit_chartAscii = assertBool "uses *" (T.any (== '*') (T.concat (lineChart a 12 2 (map Just [1 .. 12]))))
unit_chartWidths = do
  let vs = [Just 1, Nothing, Just 5, Just 3]
  mapM_ (\g -> map T.length (lineChart g 7 3 vs) @?= [7, 7, 7]) [u, a]
unit_chartEmpty = lineChart u 4 2 [] @?= ["    ", "    "]

unit_dashApprox :: Assertion
unit_dashApprox = do
  (gDash u, gApprox u) @?= ("\8212", "\8776")
  (gDash a, gApprox a) @?= ("-", "~")
