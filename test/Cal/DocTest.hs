module Cal.DocTest where

import qualified Data.Text as T
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

unit_alignRCuts :: Assertion
unit_alignRCuts = do
  let b = alignR 4 (vcat [str (st RText) "ab", str (st RText) "abcdef"])
  plainText b @?= "  ab\nabc…"
  bWidth b @?= 4

-- | A 3x3 grid of 1-column cells; the middle cell's border takes the
-- cursor style, the text inside it doesn't.
unit_cursorRect :: Assertion
unit_cursorRect = do
  let r = st RRule
      rule = line [Seg r "+-+-+"]
      row = line [Seg r "|", Seg (st RText) "a", Seg r "|", Seg (st RText) "b", Seg r "|"]
      b = vcat [rule, row, rule]
      out = cursorRect (0, 2) (2, 4) b
      roles = [ [ stRole s | Seg s t <- ss, _ <- T.unpack t ] | Line ss <- bLines out ]
      c = RCursorBorder
  roles @?= [ [RRule, RRule, c, c, c], [RRule, RText, c, RText, c], [RRule, RRule, c, c, c] ]
  plainText out @?= plainText b

unit_boxedIn :: Assertion
unit_boxedIn = do
  let b = boxedIn cursorBorder (glyphsFor GlyphsAscii) Nothing 4 (str (st RText) "x")
      borderRoles = [ stRole s | Line ss <- bLines b, Seg s t <- ss, t /= "x", T.any (/= ' ') t ]
  plainText b @?= "+--+\n|x |\n+--+"
  assertBool (show borderRoles) (all (== RCursorBorder) borderRoles)

unit_ansiBackground :: Assertion
unit_ansiBackground =
  renderAnsi ColorTrue (str (onBg RExp 100 (st RInc)) "\9600")
    @?= "\ESC[38;2;108;182;255m\ESC[48;2;244;112;103m\9600\ESC[0m"
