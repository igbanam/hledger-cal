-- | Render context, width tiers and the document every view produces. Pure.
module Cal.View.Types
  ( Tier (..), tierFor
  , RenderCtx (..), mkRenderCtx
  , ViewDoc (..), printBlock
  ) where

import Data.Text (Text)

import Cal.Doc (Block (..), Line (..), alignR, vcat)
import Cal.Format (Fmt, mkFmt)
import Cal.Glyphs (Glyphs, glyphsFor)
import Cal.Model.Common (Env (..))
import Cal.Types (Settings (..))

-- | Width tiers (spec §10.5).
data Tier = Tier80 | Tier100 | Tier120 | TierFull deriving (Eq, Ord, Show)

tierFor :: Int -> Maybe Tier
tierFor w
  | w < 80 = Nothing
  | w < 100 = Just Tier80
  | w < 120 = Just Tier100
  | w < 140 = Just Tier120
  | otherwise = Just TierFull

data RenderCtx = RenderCtx
  { rcSettings :: !Settings, rcFmt :: !Fmt, rcGlyphs :: !Glyphs, rcWidth :: !Int
  , rcTier :: !Tier, rcInteractive :: !Bool, rcMessage :: !(Maybe Text) }

-- | Nothing when the width is below 80 columns.
mkRenderCtx :: Env -> Int -> Bool -> Maybe RenderCtx
mkRenderCtx env w interactive = do
  tier <- tierFor w
  let s = envSettings env
      g = glyphsFor (sGlyphSet s)
  pure RenderCtx
    { rcSettings = s, rcFmt = mkFmt (envJournal env) (sValuation s) g, rcGlyphs = g
    , rcWidth = w, rcTier = tier, rcInteractive = interactive, rcMessage = Nothing }

-- | 'vdCursorRow' is the body row the viewport follows; 'vdCursorLast',
-- when set, is the last row of the item under the cursor, which the
-- viewport keeps in view with it when the cursor box is too tall to show
-- whole (Week: the selected transaction inside a long day column).
-- 'vdStretch', when set, is @(i, l)@: on a screen taller than the body the
-- TUI fills the spare rows with copies of @l@ before body row @i@ (Week:
-- the day columns run down to the footer). Print has no height and
-- ignores it.
data ViewDoc = ViewDoc
  { vdTop :: !Block, vdBody :: !Block, vdCursorRow :: !(Maybe Int), vdCursorLast :: !(Maybe Int)
  , vdStretch :: !(Maybe (Int, Line))
  , vdBottom :: !Block, vdKeys :: ![(Text, Text)], vdLegend :: !Block }

-- | Top, body, bottom and the legend with one blank line between; empty
-- parts are skipped. The legend sits on the right, as in the TUI footer;
-- static output has no key hints (spec §8).
printBlock :: RenderCtx -> ViewDoc -> Block
printBlock ctx d = vcat (sepBlank (filter (not . null . bLines) [vdTop d, vdBody d, vdBottom d, legend]))
  where
    lg = vdLegend d
    legend = if bWidth lg <= rcWidth ctx then alignR (rcWidth ctx) lg else lg
    blank = Block 0 [Line []]
    sepBlank [] = []
    sepBlank [x] = [x]
    sepBlank (x : xs) = x : blank : sepBlank xs
