-- | A tiny styled-text block layout language and its renderers. Pure.
module Cal.Doc
  ( Style (..), st, bold, inverse, tinted, onBg, cursorBorder
  , Seg (..), Line (..), Block (..)
  , textW, str, line, emptyB, hcat, vcat, padTo, alignR, boxed, boxedIn, cursorRect
  , truncEnd, truncAccount, wrapFig, wrapWords, plainText, renderAnsi
  ) where

import Data.Char (GeneralCategory (..), generalCategory, isAlpha, isDigit, isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Hledger.Data.AccountName (elideAccountName)
import Hledger.Data.Types (AccountName)
import Text.DocLayout (realLength)

import Cal.Glyphs (BoxChars (..), Glyphs (..))
import Cal.Theme
import Cal.Types (ColorMode (..))

-- | A foreground role at a tint, bold, reverse video, and an optional
-- background role at a tint (ignored under reverse video).
data Style = Style
  { stRole :: !Role, stTint :: !Int, stBold :: !Bool, stInverse :: !Bool
  , stBg :: !(Maybe (Role, Int)) }
  deriving (Eq, Show)

st :: Role -> Style
st r = Style r 100 False False Nothing

bold, inverse :: Style -> Style
bold s = s { stBold = True }
inverse s = s { stInverse = True }

tinted :: Int -> Style -> Style
tinted p s = s { stTint = p }

-- | Paint the cell background with a role at a tint.
onBg :: Role -> Int -> Style -> Style
onBg r p s = s { stBg = Just (r, p) }

-- | The border of the box or grid cell under the cursor (spec §10.2):
-- the accent tint, and bold where there is no colour.
cursorBorder :: Style
cursorBorder = st RCursorBorder

data Seg = Seg !Style !Text deriving (Eq, Show)

newtype Line = Line [Seg] deriving (Eq, Show)

data Block = Block { bWidth :: !Int, bLines :: ![Line] } deriving (Eq, Show)

textW :: Text -> Int
textW = realLength

segW :: Seg -> Int
segW (Seg _ t) = textW t

lineW :: Line -> Int
lineW (Line ss) = sum (map segW ss)

str :: Style -> Text -> Block
str s t = line [Seg s t]

line :: [Seg] -> Block
line ss = let l = Line ss in Block (lineW l) [l]

emptyB :: Block
emptyB = Block 0 []

-- | Pad a line with unstyled spaces up to width @n@.
padLine :: Int -> Line -> Line
padLine n l@(Line ss)
  | w >= n = l
  | otherwise = Line (ss ++ [Seg (st RText) (T.replicate (n - w) " ")])
  where w = lineW l

hcat :: Int -> [Block] -> Block
hcat _ [] = emptyB
hcat gap bs = Block width [ Line (concat (sepBy [ cells b i | b <- bs ])) | i <- [0 .. h - 1] ]
  where
    h = maximum (map (length . bLines) bs)
    width = sum (map bWidth bs) + gap * (length bs - 1)
    cells b i = case drop i (bLines (padB b)) of
      (Line ss : _) -> ss
      [] -> [Seg (st RText) (T.replicate (bWidth b) " ")]
    padB b = b { bLines = map (padLine (bWidth b)) (bLines b) }
    sepBy [] = []
    sepBy [x] = [x]
    sepBy (x : xs) = x : [Seg (st RText) (T.replicate gap " ") | gap > 0] : sepBy xs

vcat :: [Block] -> Block
vcat bs = Block (maximum (0 : map bWidth bs)) (concatMap bLines bs)

-- | Pad each line to @n@ columns; wider lines are cut and end with an ellipsis.
padTo :: Int -> Block -> Block
padTo n b = Block n (map fit (bLines b))
  where
    fit l
      | lineW l <= n = padLine n l
      | otherwise = padLine n (cutLine n l)

cutLine :: Int -> Line -> Line
cutLine n (Line ss) = Line (go (max 0 (n - 1)) ss)
  where
    go _ [] = []
    go room (Seg s t : rest)
      | w < room = Seg s t : go (room - w) rest
      | otherwise = [Seg s (takeW room t <> "…")]
      where w = textW t

-- | Longest prefix of at most @n@ columns.
takeW :: Int -> Text -> Text
takeW n t = T.pack (go 0 (T.unpack t))
  where
    go _ [] = []
    go acc (c : cs)
      | acc + cw > n = []
      | otherwise = c : go (acc + cw) cs
      where cw = textW (T.singleton c)

-- | Right-align each line in @n@ columns; wider lines are cut as in 'padTo'.
alignR :: Int -> Block -> Block
alignR n b = Block n (map f (bLines b))
  where
    f l@(Line ss) = let w = lineW l in
      if w > n then padLine n (cutLine n l)
      else if w == n then l
      else Line (Seg (st RText) (T.replicate (n - w) " ") : ss)

-- | @b@ in a box @n@ columns wide, with an optional title in the top border.
boxed :: Glyphs -> Maybe Text -> Int -> Block -> Block
boxed = boxedIn (st RRule)

-- | 'boxed' with the border in the given style ('cursorBorder' for the box
-- under the cursor).
boxedIn :: Style -> Glyphs -> Maybe Text -> Int -> Block -> Block
boxedIn rs g title n b = Block n (top : map side (bLines (padTo inner b)) ++ [bottom])
  where
    bx = gBox g
    inner = max 0 (n - 2)
    rule k = T.replicate k (T.singleton (bxH bx))
    top = Line $ case title of
      Nothing -> [Seg rs (T.singleton (bxTL bx) <> rule inner <> T.singleton (bxTR bx))]
      Just t ->
        let t' = truncEnd g (max 0 (inner - 1)) t
            fill = max 0 (inner - 1 - textW t')
        in [ Seg rs (T.singleton (bxTL bx) <> rule 1), Seg (st RDim) t'
           , Seg rs (rule fill <> T.singleton (bxTR bx)) ]
    side (Line ss) = Line (Seg rs (T.singleton (bxV bx)) : ss ++ [Seg rs (T.singleton (bxV bx))])
    bottom = Line [Seg rs (T.singleton (bxBL bx) <> rule inner <> T.singleton (bxBR bx))]

-- | @cursorRect (r0, c0) (r1, c1) b@: the grid lines on the perimeter of
-- the rectangle from line @r0@, column @c0@ to line @r1@, column @c1@
-- (inclusive, 0-based) take the 'cursorBorder' style. Only cells drawn in
-- the rule role change, so text on the perimeter keeps its style and the
-- block keeps its size. Used for a grid cell whose borders are shared
-- with its neighbours.
cursorRect :: (Int, Int) -> (Int, Int) -> Block -> Block
cursorRect (r0, c0) (r1, c1) b = b { bLines = zipWith restyle [0 ..] (bLines b) }
  where
    restyle i l@(Line ss)
      | i < r0 || i > r1 = l
      | i == r0 || i == r1 = Line (go 0 (\c -> c >= c0 && c <= c1) ss)
      | otherwise = Line (go 0 (\c -> c == c0 || c == c1) ss)
    go _ _ [] = []
    go col on (Seg s t : rest)
      | stRole s /= RRule || not (any on [col .. col + w - 1]) = Seg s t : go (col + w) on rest
      | otherwise = runs col (T.unpack t) ++ go (col + w) on rest
      where
        w = textW t
        runs _ [] = []
        runs c cs@(_ : _) =
          let flag = on c
              (run, more) = spanW flag c cs
          in Seg (if flag then cursorBorder else s) (T.pack run) : runs (c + textW (T.pack run)) more
        spanW _ _ [] = ([], [])
        spanW flag c (x : xs)
          | on c == flag = let (a, z) = spanW flag (c + textW (T.singleton x)) xs in (x : a, z)
          | otherwise = ([], x : xs)

truncEnd :: Glyphs -> Int -> Text -> Text
truncEnd g n t
  | textW t <= n = t
  | otherwise = takeW (max 0 (n - textW (gOpen g))) t <> gOpen g

-- | A figure split over lines of at most @n@ columns: between amounts
-- first (an operator stays with the amount after it, a commodity code with
-- its number: "− KD 5.000", "− 0.01 BTC"), then after a digit-group comma;
-- figures are wrapped, never cut. Used wherever a figure may not fit
-- (cards and their sub-lines, month cells, week nets, hints).
wrapFig :: Glyphs -> Int -> Text -> [Text]
wrapFig g n t
  | n <= 0 || textW t <= n = [t]
  | otherwise = concatMap fit (fill " " (withCodes (glue (T.words t))))
  where
    -- "+" / "−" / "-" words join the word after them
    glue (o : x : xs) | o `elem` ["+", gMinus g] = glue ((o <> " " <> x) : xs)
    glue (x : xs) = x : glue xs
    glue [] = []
    withCodes (x : y : xs) | codeThenNumber x y || numberThenCode x y = withCodes ((x <> " " <> y) : xs)
    withCodes (x : xs) = x : withCodes xs
    withCodes [] = []
    lastWord x = if T.null x then x else last (T.words x)
    codeThenNumber x y =
      let w = lastWord x
      in not (T.null w) && T.all (\c -> isAlpha c || generalCategory c == CurrencySymbol) w
           && maybe False (isDigit . fst) (T.uncons y)
    numberThenCode x y = T.any isDigit (lastWord x) && T.length y >= 2 && T.all isUpper y
    fit l
      | textW l <= n = [l]
      | otherwise = fill "" (concatMap hard (commaParts l))
    commaParts x = case T.breakOn "," x of
      (a, b) | T.null b -> [a]
             | otherwise -> (a <> ",") : commaParts (T.drop 1 b)
    hard tok = if textW tok > n then T.chunksOf n tok else [tok]
    fill _ [] = []
    fill sep (x : xs) = go x xs
      where
        go cur [] = [cur]
        go cur (y : ys)
          | textW cur + textW sep + textW y <= n = go (cur <> sep <> y) ys
          | otherwise = cur : go y ys

-- | Word-wrap @t@ into lines of at most @n@ columns; continuation lines
-- get @indent@ spaces. A word wider than a line is broken into pieces that
-- fit, so no line is ever wider than @n@.
wrapWords :: Int -> Int -> Text -> [Text]
wrapWords n indent t = case concatMap split (T.words t) of
  [] -> [""]
  (x : xs) -> go x xs
  where
    pre = T.replicate indent " "
    room = max 1 (n - indent)
    split x
      | textW x <= room = [x]
      | otherwise = let h = takeW room x
                        h' = if T.null h then T.take 1 x else h
                    in h' : split (T.drop (T.length h') x)
    go cur [] = [cur]
    go cur (y : ys)
      | textW cur + 1 + textW y <= n = go (cur <> " " <> y) ys
      | otherwise = cur : go (pre <> y) ys

truncAccount :: Int -> AccountName -> Text
truncAccount = elideAccountName

plainText :: Block -> Text
plainText b = T.intercalate "\n" [ T.dropWhileEnd (== ' ') (T.concat [ t | Seg _ t <- ss ]) | Line ss <- bLines b ]

renderAnsi :: ColorMode -> Block -> Text
renderAnsi ColorNone b = plainText b
renderAnsi cm b = T.intercalate "\n" (map renderLine (bLines b))
  where
    renderLine (Line ss) = T.concat (map renderSeg ss)
    renderSeg (Seg s t)
      | T.null t = ""
      | T.all (== ' ') t && not (stInverse s) && stBg s == Nothing = t
      | otherwise = T.concat [boldP, colour, t, "\ESC[0m"]
      where
        boldP = if stBold s then "\ESC[1m" else ""
        c = tintRGB (stTint s) (roleRGB (stRole s))
        colour
          | stInverse s = ansiBg cm (roleRGB (stRole s)) <> ansiFg cm backgroundRGB
          | otherwise = fg <> maybe "" (\(r, p) -> ansiBg cm (tintRGB p (roleRGB r))) (stBg s)
        fg
          | stTint s == 100 = ansiFg cm (roleRGB (stRole s))
          | otherwise = ansiFg cm c
