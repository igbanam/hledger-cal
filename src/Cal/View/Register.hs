-- | The register pane (§10.3): one day's postings with a running total.
module Cal.View.Register (registerPane, registerVisibleRows) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)

import Cal.Currency (codeOf, symbolOf)
import Cal.Doc
import Cal.Format (Fmt (..), fmtDayLong, fmtNative, fmtRate, fmtSigned)
import Cal.Glyphs (Glyphs (..))
import Cal.Model.Register (RegisterRow (..))
import Cal.Types
import Cal.View.Chrome (colW, dim, plain, signStyle, spaces)
import Cal.View.Types

-- | Posting rows that fit in a pane of the given height (title, border and
-- footer take the other three lines).
registerVisibleRows :: Int -> Int
registerVisibleRows height = max 0 (height - 3)

-- | @registerPane ctx day height scroll rows@: a box of exactly @height@ lines.
registerPane :: RenderCtx -> Day -> Int -> Int -> [RegisterRow] -> Block
registerPane ctx day height scroll rows = Block w (take height (bLines boxedB ++ repeat (Line [])))
  where
    g = rcGlyphs ctx
    f = rcFmt ctx
    s = rcSettings ctx
    w = rcWidth ctx
    sep = gSep g
    n = length rows

    title = truncEnd g (max 0 (w - 3)) $
      "Register" <> sep <> fmtDayLong day <> sep <> T.pack (show n) <> (if n == 1 then " posting" else " postings") <> sep <> mode
    mode = case sValuation s of
      Native -> "native"
      ValuedIn b -> "valued in " <> codeOf b <> " at " <> rateAtName (sRateAt s)
    rateAtName AtPeriodEnd = "period end"
    rateAtName AtTxnDate = "txn date"
    rateAtName AtToday = "today"

    baseSym = maybe "" (\b -> fromMaybe b (symbolOf b)) (fBase f)
    dateT r = case rgDate r of
      Just d -> (if rgForecast r then "~" else "") <> T.pack (formatTime defaultTimeLocale "%m-%d" d)
      Nothing -> ""
    nativeT = fmtNative f . rgNative
    rateT r = maybe "native" (\q -> "@ " <> baseSym <> fmtRate q) (rgRate r)
    valueT = fmtSigned f . rgValue
    runT = fmtSigned f . rgRunning

    dateW = colW (textW . dateT) rows
    natW = colW (textW . nativeT) rows
    rateW = colW (textW . rateT) rows
    valW = colW (textW . valueT) rows
    runW = colW (textW . runT) rows
    descNat = colW (maybe 0 textW . rgDescription) rows
    acctNat = colW (textW . rgAccount) rows

    inner = max 0 (w - 2)
    cw = max 0 (inner - 2)
    gaps = 2 * 6
    avail = max 0 (cw - dateW - natW - rateW - valW - runW - gaps)
    acctW = min avail (max (min acctNat 8) (min acctNat (avail - descNat)))
    descW = max 0 (min descNat (avail - acctW))

    visible = registerVisibleRows height
    shown = take visible (drop (max 0 scroll) rows)

    trunc k t = if k <= 0 then "" else truncEnd g k t
    cell :: Int -> Text -> Text
    cell k t = t <> T.replicate (k - textW t) " "
    cellR k t = T.replicate (k - textW t) " " <> t

    rowLine r =
      let valT = valueT r
          runTxt = runT r
          natSt = if rgForecast r then dim else plain
      in [ spaces 1, Seg dim (cell dateW (dateT r)), spaces 2
         , Seg plain (cell descW (maybe "" (trunc descW) (rgDescription r))), spaces 2
         , Seg plain (cell acctW (trunc acctW (truncAccount acctW (rgAccount r)))), spaces 2
         , Seg natSt (cellR natW (nativeT r)), spaces 2
         , Seg dim (cellR rateW (rateT r)), spaces 2
         , Seg (signStyle g valT) (cellR valW valT), spaces 2
         , Seg (signStyle g runTxt) (cellR runW runTxt) ]

    footerText = gKeyUpDown g <> " scroll  Esc back"
    footerLine = [spaces 1, Seg dim footerText]
    padLines = replicate (visible - length shown) (Line [])
    body = Block inner (map (Line . rowLine) shown ++ padLines ++ [Line footerLine])
    boxedB = boxed g (Just title) w body
