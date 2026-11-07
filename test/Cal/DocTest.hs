module Cal.DocTest where

import Test.Tasty.HUnit

import Cal.Doc
import Cal.Glyphs (glyphsFor)
import Cal.Theme
import Cal.Types (ColorMode (..), GlyphSet (..))

unit_hcatPads, unit_widthWide, unit_padTrunc, unit_boxed, unit_ansiTrue, unit_ansiNone, unit_tint, unit_acct, unit_256 :: Assertion
unit_hcatPads = plainText (hcat 1 [str (st RText) "ab", vcat [str (st RText) "c", str (st RText) "d"]]) @?= "ab c\n   d"
unit_widthWide = textW "₦€—▲" @?= 4
unit_padTrunc = plainText (padTo 5 (str (st RText) "abcdefgh")) @?= "abcd…"
unit_boxed = plainText (boxed (glyphsFor GlyphsAscii) (Just "T") 6 (str (st RText) "x"))
  @?= "+-T--+\n|x   |\n+----+"
unit_ansiTrue = renderAnsi ColorTrue (str (st RInc) "a") @?= "\ESC[38;2;108;182;255ma\ESC[0m"
unit_ansiNone = renderAnsi ColorNone (str (bold (st RExp)) "a") @?= "a"
unit_tint = tintRGB 60 (roleRGB RExp) @?= (152, 74, 70)
unit_acct = truncAccount 20 "expenses:household:maintenance:plumbing" @?= "ex:ho:ma:plumbing"
unit_256 = ansiFg Color256 (roleRGB RInc) @?= "\ESC[38;5;75m"
