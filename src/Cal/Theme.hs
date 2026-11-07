-- | Role colours and terminal colour escapes. Pure.
module Cal.Theme
  ( Role (..), RGB, roleRGB, tintRGB, ansiFg, ansiBg, backgroundRGB
  ) where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)

import Cal.Types (ColorMode (..))

data Role = RText | RDim | RFaded | RInc | RExp | RCursor | RWarn | RRule | RPanel
  deriving (Eq, Ord, Show, Enum, Bounded)

type RGB = (Word8, Word8, Word8)

-- | The page background, #0d1014.
backgroundRGB :: RGB
backgroundRGB = (13, 16, 20)

roleRGB :: Role -> RGB
roleRGB r = case r of
  RPanel -> (0x13, 0x17, 0x1c)
  RRule -> (0x2a, 0x2f, 0x37)
  RText -> (0xd6, 0xd9, 0xde)
  RDim -> (0x8b, 0x94, 0x9e)
  RFaded -> (0x4b, 0x52, 0x5c)
  RInc -> (0x6c, 0xb6, 0xff)
  RExp -> (0xF4, 0x70, 0x67)
  RCursor -> (0xB0, 0x83, 0xF0)
  RWarn -> (0xDA, 0xAA, 0x3F)

-- | @tintRGB p c@: @p@ percent of @c@ mixed into the background.
tintRGB :: Int -> RGB -> RGB
tintRGB p (r, g, b) = (mix bR r, mix bG g, mix bB b)
  where
    (bR, bG, bB) = backgroundRGB
    mix :: Word8 -> Word8 -> Word8
    mix bg c = round (fromIntegral bg + (fromIntegral c - fromIntegral bg) * fromIntegral p / (100 :: Double))

-- | Full SGR sequences; empty for 'ColorNone'.
ansiFg, ansiBg :: ColorMode -> RGB -> Text
ansiFg = ansiColor 38
ansiBg = ansiColor 48

ansiColor :: Int -> ColorMode -> RGB -> Text
ansiColor _ ColorNone _ = ""
ansiColor base ColorTrue (r, g, b) =
  sgr [T.pack (show base), "2", num r, num g, num b]
  where num = T.pack . show
ansiColor base Color256 c = sgr [T.pack (show base), "5", T.pack (show (nearest256 c))]

sgr :: [Text] -> Text
sgr ps = "\ESC[" <> T.intercalate ";" ps <> "m"

-- | Nearest xterm-256 index (6x6x6 cube or 24 greys) by squared RGB distance.
nearest256 :: RGB -> Int
nearest256 (r, g, b) = fst (minimumBy (comparing snd) (cube ++ greys))
  where
    levels = [0, 95, 135, 175, 215, 255] :: [Int]
    px = (fromIntegral r, fromIntegral g, fromIntegral b) :: (Int, Int, Int)
    dist (a, b', c) (a', b'', c') = (a - a') ^ (2 :: Int) + (b' - b'') ^ (2 :: Int) + (c - c') ^ (2 :: Int)
    cube = [ (16 + 36 * i + 6 * j + k, dist px (x, y, z))
           | (i, x) <- zip [0 ..] levels, (j, y) <- zip [0 ..] levels, (k, z) <- zip [0 ..] levels ]
    greys = [ (232 + n, dist px (v, v, v)) | n <- [0 .. 23], let v = 8 + 10 * n ]
