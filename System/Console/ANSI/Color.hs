-- | Conversions between color representations
module System.Console.ANSI.Color
  (
  -- | 8-bit to 24-bit color conversion
    xterm256ToSRGB
  -- | Utility functions
  , clamp
  , closest4bitsANSIColor
  , closestXterm256Color
  , color8CodeToXterm256
  , colorToCode
  , xterm256ColorToCode
    ) where


import Data.Colour.SRGB (Colour, RGB (..), sRGB, toSRGB)

import Data.Word (Word8)
import Data.Colour.Names (black, blue, cyan, green, grey, lime, magenta, maroon,
                          navy, olive, purple, red, silver, teal, white, yellow)

import Data.List (minimumBy, (!!))
import Data.Maybe (fromJust)

import System.Console.ANSI.Types

-- | Returns a color, in the sRGB colorspace, whose components are the
-- components of an 8-bit ANSI color.
-- For each color component, we first clamp the value to its admissible range
-- defined in https://en.wikipedia.org/wiki/ANSI_escape_code#Colors:
--  - 0-5 for 8-bit rgb
--  - 0-23 for 8-bit grayscale
-- Then we apply the xterm mapping (the mapped value is in range [0..255]).
-- Finally we interpret the xterm-mapped values as components of a color
-- in srgb colorspace (we divide by 255 to express the value as a
-- floating point in range [0 1]).
xterm256ToSRGB :: Xterm256Color -> Colour Float
xterm256ToSRGB (SystemColor intensity color)
  = fromJust $ lookup (intensity, color) aNSIColors
xterm256ToSRGB (RGBColor (RGB r' g' b')) = sRGB r g b
 where
  mapping :: Word8 -> Float
  mapping = (/ 255.0) . fromIntegral . xtermMapRGB8bitComponent . (\v -> clamp v 0 5)
  r = mapping r'
  g = mapping g'
  b = mapping b'
xterm256ToSRGB (GrayColor y') = sRGB y y y
 where
  mapping :: Word8 -> Float
  mapping = (/ 255.0) . fromIntegral . xtermMapGray8bitComponent . (\v -> clamp v 0 23)
  y = mapping y'

-- | how xterm interprets 8bit rgb colors (deduced from https://jonasjacek.github.io/colors/)
xtermMapRGB8bitComponent :: Word8
                         -- ^ input values are in range [0..5]
                         -- (the admissible range for rgb components of 8 bit
                         -- rgb ANSI colors, cf.
                         -- https://en.wikipedia.org/wiki/ANSI_escape_code#Colors)
                         -> Word8
                         -- ^ output is in range [0..255]
xtermMapRGB8bitComponent 0 = 0
xtermMapRGB8bitComponent n = 55 + n * 40

-- | how xterm interprets 8bit grayscale colors (deduced from https://jonasjacek.github.io/colors/)
xtermMapGray8bitComponent :: Word8
                         -- ^ input values are in range [0..23]
                         -- (the admissible range for gray component of 8 bit
                         -- grayscale ANSI colors, cf.
                         -- https://en.wikipedia.org/wiki/ANSI_escape_code#Colors)
                          -> Word8
                          -- ^ output is in range [0..255]
xtermMapGray8bitComponent v = 8 + 10 * v

-- | clamps a number to a range.
clamp :: Num a => Ord a =>
         a
      -- ^ the value
      -> a
      -- ^ the inclusive minimum bound
      -> a
      -- ^ the inclusive maximum bound
      -> a
clamp n min_ max_
  | n < min_ = min_
  | n > max_ = max_
  | otherwise = n

-- | a Color8Code represents the escape code for an 8-bit ANSI Color. It is in
-- range [0..255]. The ranges of its values are:
-- (from https://en.wikipedia.org/wiki/ANSI_escape_code#Colors)
--
-- 0x00-0x07:  standard colors (as in ESC [ 30–37 m)
-- 0x08-0x0F:  high intensity colors (as in ESC [ 90–97 m)
-- 0x10-0xE7:  6 × 6 × 6 cube (216 colors):
--             16 + 36 × r + 6 × g + b (0 ≤ r, g, b ≤ 5)
-- 0xE8-0xFF:  grayscale from black to white in 24 steps
--
-- The colors described by the 2 first ranges (0x00-0x07 and 0x08-0x0F) match
-- exactly with the 4 bit ANSI colors represented by (ColorIntensity, Color).
-- The colors of the 3rd range (0x10-0xE7) are represented by RGB8Color, and the
-- colors of the 4th range are represented by Gray8Color.
color8CodeToXterm256 :: Color8Code -> Xterm256Color
color8CodeToXterm256 (Color8Code c)
  | c < 16    = SystemColor intensity color -- interpreted as a 4-bit ANSI color
  | c < 232   = RGBColor $ asRGB (c - 16)   -- interpreted as 8-bit rgb
  | otherwise = GrayColor (c - 232)         -- interpreted as 8-bit grayscale
 where
  (intensity, color) = fst $ aNSIColors !! fromIntegral c
  asRGB i = let -- we know that i = 36 × r + 6 × g + b and (0 ≤ r, g, b ≤ 5)
                -- (cf. comment on top) so we can deduce the unique set of
                -- corresponding r g and b values:
                r = quot i 36
                g = quot (i - 36 * r) 6
                b = i - (6 * g + 36 * r)
            in  RGB r g b

-- | Color8 represents an 8-bit ANSI color. This function converts it to the
-- corresponding code, defined by https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
--
-- For safety the values of Color8 are clamped in their respective ranges.
xterm256ColorToCode :: Xterm256Color -> Color8Code
xterm256ColorToCode (SystemColor Dull color)
  = Color8Code (fromIntegral $ colorToCode color)
xterm256ColorToCode (SystemColor Vivid color)
  = Color8Code (fromIntegral $ 8 + colorToCode color)
-- 8-bit rgb colors are represented by code:
-- 16 + 36 × r + 6 × g + b (0 ≤ r, g, b ≤ 5) (see link to spec above)
xterm256ColorToCode (RGBColor (RGB r' g' b'))
  = Color8Code (16 + 36 * r + 6 * g + b)
  where
    clamp' x = clamp x 0 5
    r = clamp' r'
    g = clamp' g'
    b = clamp' b'
-- 8-bit grayscale colors are represented by code: 232 + g (g in [0..23]) (see
-- link to spec above)
xterm256ColorToCode (GrayColor y) = Color8Code (232 + clamp y 0 23)

-- | Finds the color in the xterm 256 color protocol that is closest in srgb
-- colorspace to a given color in that colorspace
closestXterm256Color :: Colour Float -> Xterm256Color
closestXterm256Color = closestColor xtermColors
 where
  xterm256colors = map (\c -> color8CodeToXterm256 (Color8Code c)) [0 .. 255]
  xtermColors = map (\x -> (x, xterm256ToSRGB x)) xterm256colors

-- | Finds the 4 bit color that is closest in srgb colorspace to a given color
-- in that space
closest4bitsANSIColor :: Colour Float -> (ColorIntensity, Color)
closest4bitsANSIColor = closestColor aNSIColors

-- | Finds the color that has the closest equivalent color in srgb colorspace to
-- a given color in that space
closestColor :: [(a, Colour Float)] -> Colour Float -> a
closestColor colors color = fst $ minimumBy order colors
 where
  RGB r g b = toSRGB color
  order (_, c1) (_, c2) = compare (dist c1) (dist c2)
  dist c = let RGB r' g' b' = toSRGB c
               dr = r' - r
               dg = g' - g
               db = b' - b
           in  dr * dr + dg * dg + db * db

-- | Maps 4-bit ANSI colors to the equivalent constructors of SRGB.Colour
aNSIColors :: [((ColorIntensity, Color), Colour Float)]
aNSIColors = [ ((Dull,  Black),   black)
             , ((Dull,  Blue),    navy)
             , ((Dull,  Green),   green)
             , ((Dull,  Cyan),    teal)
             , ((Dull,  Red),     maroon)
             , ((Dull,  Magenta), purple)
             , ((Dull,  Yellow),  olive)
             , ((Dull,  White),   silver)
             , ((Vivid, Black),   grey)
             , ((Vivid, Blue),    blue)
             , ((Vivid, Green),   lime)
             , ((Vivid, Cyan),    cyan)
             , ((Vivid, Red),     red)
             , ((Vivid, Magenta), magenta)
             , ((Vivid, Yellow),  yellow)
             , ((Vivid, White),   white) ]

-- | 'colorToCode' @color@ returns the 0-based index of the color (one of the
-- eight colors in the standard).
colorToCode :: Color -> Int
colorToCode color = case color of
  Black   -> 0
  Red     -> 1
  Green   -> 2
  Yellow  -> 3
  Blue    -> 4
  Magenta -> 5
  Cyan    -> 6
  White   -> 7