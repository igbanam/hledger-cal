-- | A tiny styled-text block layout language and its renderers. Pure.
module Cal.Doc
  ( Style (..), st, bold, inverse, tinted
  , Seg (..), Line (..), Block (..)
  , textW, str, line, emptyB, hcat, vcat, padTo, alignR, boxed
  , truncEnd, truncAccount, plainText, renderAnsi
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Hledger.Data.AccountName (elideAccountName)
import Hledger.Data.Types (AccountName)
import Text.DocLayout (realLength)

import Cal.Glyphs (BoxChars (..), Glyphs (..))
import Cal.Theme
import Cal.Types (ColorMode (..))

data Style = Style { stRole :: !Role, stTint :: !Int, stBold :: !Bool, stInverse :: !Bool }
  deriving (Eq, Show)

st :: Role -> Style
st r = Style r 100 False False

bold, inverse :: Style -> Style
bold s = s { stBold = True }
inverse s = s { stInverse = True }

tinted :: Int -> Style -> Style
tinted p s = s { stTint = p }

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

alignR :: Int -> Block -> Block
alignR n b = Block n (map f (bLines b))
  where
    f l@(Line ss) = let w = lineW l in
      if w >= n then l else Line (Seg (st RText) (T.replicate (n - w) " ") : ss)

boxed :: Glyphs -> Maybe Text -> Int -> Block -> Block
boxed g title n b = Block n (top : map side (bLines (padTo inner b)) ++ [bottom])
  where
    bx = gBox g
    inner = max 0 (n - 2)
    rule k = T.replicate k (T.singleton (bxH bx))
    rs = st RRule
    top = Line $ case title of
      Nothing -> [Seg rs (T.singleton (bxTL bx) <> rule inner <> T.singleton (bxTR bx))]
      Just t ->
        let t' = truncEnd g (max 0 (inner - 1)) t
            fill = max 0 (inner - 1 - textW t')
        in [ Seg rs (T.singleton (bxTL bx) <> rule 1), Seg (st RDim) t'
           , Seg rs (rule fill <> T.singleton (bxTR bx)) ]
    side (Line ss) = Line (Seg rs (T.singleton (bxV bx)) : ss ++ [Seg rs (T.singleton (bxV bx))])
    bottom = Line [Seg rs (T.singleton (bxBL bx) <> rule inner <> T.singleton (bxBR bx))]

truncEnd :: Glyphs -> Int -> Text -> Text
truncEnd g n t
  | textW t <= n = t
  | otherwise = takeW (max 0 (n - textW (gOpen g))) t <> gOpen g

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
      | T.all (== ' ') t && not (stInverse s) = t
      | otherwise = T.concat [boldP, colour, t, "\ESC[0m"]
      where
        boldP = if stBold s then "\ESC[1m" else ""
        c = tintRGB (stTint s) (roleRGB (stRole s))
        colour
          | stInverse s = ansiBg cm (roleRGB (stRole s)) <> ansiFg cm backgroundRGB
          | stTint s == 100 = ansiFg cm (roleRGB (stRole s))
          | otherwise = ansiFg cm c
