-- | Glyph sets (UTF-8 / ASCII) and small text graphics. Pure.
module Cal.Glyphs
  ( BoxChars (..), Glyphs (..), glyphsFor, hbar, vbar, sparkline, lineChart
  ) where

import Data.Bits (setBit)
import Data.Char (chr)
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Cal.Types (GlyphSet (..))

data BoxChars = BoxChars
  { bxH, bxV, bxTL, bxTR, bxBL, bxBR, bxLT, bxRT, bxTT, bxBT, bxX :: !Char }
  deriving (Eq, Show)

data Glyphs = Glyphs
  { gFull :: !Char, gEighths :: ![Char], gSpark :: ![Char], gSparkMissing :: !Char
  , gChartPoint :: !(Maybe Char)
  , gLeft, gRight, gOverspent, gStale, gOpen, gSep, gMinus, gLegendBar
  , gCursorMark, gArrow, gTimes, gDash, gApprox :: !Text
  , gKeyArrows, gKeyUpDown, gKeyLeftRight, gKeyEnter, gDelta :: !Text  -- ^ key hints, Δ
  , gMixRank :: ![Char]  -- ^ mix-bar segments by rank, where there is no colour
  , gBarThin, gBarTrack, gBarTrackPlain :: !Char
    -- ^ thin bar, its track in colour, its track without colour
  , gBarPair :: !(Maybe Char)  -- ^ one cell holding two stacked bars (fg over bg)
  , gColEighths :: ![Char]  -- ^ lower eighths for the top of a column bar (none in ASCII)
  , gHairline :: !Char  -- ^ a column bar too short to show one eighth
  , gDashRule :: !Char  -- ^ dashed rule between items
  , gPrompt :: !Text  -- ^ the currency picker's filter prompt
  , gBox :: !BoxChars }
  deriving (Eq, Show)

glyphsFor :: GlyphSet -> Glyphs
glyphsFor GlyphsUtf8 = Glyphs
  { gFull = '█', gEighths = "▏▎▍▌▋▊▉", gSpark = "▁▂▃▄▅▆▇█", gSparkMissing = '·'
  , gChartPoint = Nothing
  , gLeft = "◀", gRight = "▶", gOverspent = "▲", gStale = "•", gOpen = "…"
  , gSep = " · ", gMinus = "−", gLegendBar = "━", gCursorMark = "▢"
  , gArrow = "→", gTimes = "×", gDash = "—", gApprox = "≈"
  , gKeyArrows = "←↑↓→", gKeyUpDown = "↑↓", gKeyLeftRight = "←→", gKeyEnter = "⏎", gDelta = "Δ"
  , gMixRank = "━═┅┄"
  , gBarThin = '━', gBarTrack = '━', gBarTrackPlain = '─', gBarPair = Just '▀'
  , gColEighths = "▁▂▃▄▅▆▇", gHairline = '▁', gDashRule = '┄', gPrompt = "▸"
  , gBox = BoxChars '─' '│' '┌' '┐' '└' '┘' '├' '┤' '┬' '┴' '┼' }
glyphsFor GlyphsAscii = Glyphs
  { gFull = '#', gEighths = [], gSpark = "_.-~^", gSparkMissing = '.'
  , gChartPoint = Just '*'
  , gLeft = "<", gRight = ">", gOverspent = "^", gStale = "*", gOpen = "..."
  , gSep = " . ", gMinus = "-", gLegendBar = "-", gCursorMark = "[]"
  , gArrow = "->", gTimes = "x", gDash = "-", gApprox = "~"
  , gKeyArrows = "arrows", gKeyUpDown = "up/down", gKeyLeftRight = "left/right", gKeyEnter = "enter", gDelta = "delta"
  , gMixRank = "#=-."
  , gBarThin = '=', gBarTrack = '-', gBarTrackPlain = '-', gBarPair = Nothing
  , gColEighths = [], gHairline = '_', gDashRule = '-', gPrompt = ">"
  , gBox = BoxChars '-' '|' '+' '+' '+' '+' '+' '+' '+' '+' '+' }

-- | Horizontal bar of @w@ cells filled to fraction @f@ (clamped 0..1); no padding.
hbar :: Glyphs -> Int -> Double -> Text
hbar g w f = T.pack (replicate full (gFull g) ++ partial)
  where
    cells = fromIntegral (max 0 w) * max 0 (min 1 f) :: Double
    whole = floor cells :: Int
    eighths = round ((cells - fromIntegral whole) * 8) :: Int
    (full, partial)
      | null (gEighths g) = (whole, [])
      | eighths >= 8 = (min (max 0 w) (whole + 1), [])
      | eighths >= 1 = (whole, [gEighths g !! (eighths - 1)])
      | otherwise = (whole, [])

-- | A bottom-aligned column bar @h@ rows tall filled to fraction @f@
-- (clamped 0..1), as one character per row, top row first: whole cells,
-- then a lower eighth for the top edge ('gColEighths'; whole cells only in
-- ASCII). A non-zero fraction too small for one eighth shows 'gHairline'
-- on the bottom row; zero is all blank.
vbar :: Glyphs -> Int -> Double -> [Char]
vbar g h f = reverse (take h (body ++ repeat ' '))
  where
    f' = max 0 (min 1 f)
    steps = if null (gColEighths g) then 1 else 8 :: Int
    n = round (fromIntegral (max 0 h * steps) * f') :: Int
    (whole, part) = n `divMod` steps
    body
      | f' > 0 && n == 0 = [gHairline g]
      | part == 0 = replicate whole (gFull g)
      | otherwise = replicate whole (gFull g) ++ [gColEighths g !! (part - 1)]

-- | One character per value, scaled min to max; missing values use 'gSparkMissing'.
sparkline :: Glyphs -> [Maybe Double] -> Text
sparkline g vs = T.pack (map glyph vs)
  where
    present = catMaybes vs
    lo = minimum present
    hi = maximum present
    glyph Nothing = gSparkMissing g
    glyph (Just v) = gSpark g !! level (if hi == lo then 3 else round ((v - lo) / (hi - lo) * 7))
    level :: Int -> Int
    level i
      | length (gSpark g) == 8 = i
      | otherwise = [0, 0, 1, 2, 2, 3, 4, 4] !! i

-- | Line chart, @w@ cells by @h@ rows; braille dots, or one @*@ per column in ASCII.
-- Always returns exactly @h@ lines of exactly @w@ characters.
lineChart :: Glyphs -> Int -> Int -> [Maybe Double] -> [Text]
lineChart g w0 h0 vals = case gChartPoint g of
  Just c -> [T.pack [ if S.member (x, y) asciiPts then c else ' ' | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]
  Nothing -> [T.pack [ cell x y | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]
  where
    w = max 0 w0
    h = max 0 h0
    present = catMaybes vals
    lo = if null present then 0 else minimum present
    hi = if null present then 0 else maximum present
    nVals = length vals
    -- sample i of n points
    sample n i = vals !! min (nVals - 1) (i * nVals `div` n)
    samples n = if nVals == 0 || n <= 0 then [] else [sample n i | i <- [0 .. n - 1]]
    rowOf rows v
      | hi == lo = (rows - 1) `div` 2
      | otherwise = round ((hi - v) / (hi - lo) * fromIntegral (rows - 1))
    asciiPts = S.fromList [ (x, rowOf h v) | (x, Just v) <- zip [0 ..] (samples w) ]
    -- braille: dot (column, row) over a 2w by 4h grid
    dotRows = [ fmap (rowOf (h * 4)) s | s <- samples (2 * w) ]
    dots = S.fromList (concat (zipWith3 fill [0 :: Int ..] (Nothing : dotRows) dotRows))
    fill x prev cur = case (prev, cur) of
      (Just p, Just c) -> [ (x, r) | r <- [min p c .. max p c] ]
      (_, Just c) -> [(x, c)]
      _ -> []
    cell cx cy = case [ bit | (dx, dy, bit) <- brailleBits, S.member (2 * cx + dx, 4 * cy + dy) dots ] of
      [] -> ' '
      bs -> chr (0x2800 + foldl setBit 0 bs)
    brailleBits = [ (0, 0, 0), (0, 1, 1), (0, 2, 2), (0, 3, 6)
                  , (1, 0, 3), (1, 1, 4), (1, 2, 5), (1, 3, 7) ] :: [(Int, Int, Int)]

