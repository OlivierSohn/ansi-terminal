-- | Conversions between color representations
module System.Console.ANSI.Color
  (
  -- | 8-bit to 24-bit color conversion
    color8To24
  -- | Utility functions
  , clamp
  , color8CodeToEither
  , closest4bitsANSIColor
    ) where


import qualified Data.Colour.SRGB.Linear as SRGBLin(rgb, Colour)
import qualified Data.Colour.SRGB as SRGB (RGB (..), toSRGB, Colour)

import Data.Colour.Names (black, blue, cyan, green, grey, lime, magenta, maroon,
                          navy, olive, purple, red, silver, teal, white, yellow)

import Data.List (minimumBy, (!!))

import System.Console.ANSI.Types

-- | Returns a color, in the linear sRGB colorspace, whose components are the components
--  of an 8-bit ANSI color.
-- For each color component, we first clamp the value to its admissible range
-- defined in https://en.wikipedia.org/wiki/ANSI_escape_code#Colors:
--  - 0-5 for 8-bit rgb
--  - 0-23 for 8-bit grayscale
-- Then we apply the xterm mapping (the mapped value is in range [0..255]).
-- Finally we interpret the xterm-mapped values as components of a color
-- in linear srgb colorspace (we divide by 255 to express the value as a floating point in range [0 1]).
color8To24 :: Color8 -> SRGBLin.Colour Float
color8To24 (RGB8Color r' g' b') =
  let mapping = (\v -> v/255.0) . fromIntegral . xtermMapRGB8bitComponent . (\v -> clamp v 0 5) :: Int -> Float
      r = mapping r'
      g = mapping g'
      b = mapping b'
  in  SRGBLin.rgb r g b
color8To24 (Gray8Color g') =
  let mapping = (\v -> v/255.0) . fromIntegral . xtermMapGray8bitComponent . (\v -> clamp v 0 23) :: Int -> Float
      g = mapping g'
  in  SRGBLin.rgb g g g

-- | how xterm interprets 8bit rgb colors (deduced from https://jonasjacek.github.io/colors/)
xtermMapRGB8bitComponent :: Int
                         -- ^ input values are in range [0..5]
                         -- (the admissible range for rgb components of 8bit rgb ANSI colors, cf.
                         -- https://en.wikipedia.org/wiki/ANSI_escape_code#Colors)
                         -> Int
                         -- ^ output is in range [0..255]
xtermMapRGB8bitComponent 0 = 0
xtermMapRGB8bitComponent n = 55 + n * 40

-- | how xterm interprets 8bit grayscale colors (deduced from https://jonasjacek.github.io/colors/)
xtermMapGray8bitComponent :: Int
                         -- ^ input values are in range [0..23]
                         -- (the admissible range for gray component of 8bit grayscale ANSI colors, cf.
                         -- https://en.wikipedia.org/wiki/ANSI_escape_code#Colors)
                          -> Int
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

-- | a Color8Code represents the escape code for an 8-bit ANSI Color. It is in range [0..255].
-- The ranges of its values are : (from https://en.wikipedia.org/wiki/ANSI_escape_code#Colors)
--
-- 0x00-0x07:  standard colors (as in ESC [ 30–37 m)
-- 0x08-0x0F:  high intensity colors (as in ESC [ 90–97 m)
-- 0x10-0xE7:  6 × 6 × 6 cube (216 colors): 16 + 36 × r + 6 × g + b (0 ≤ r, g, b ≤ 5)
-- 0xE8-0xFF:  grayscale from black to white in 24 steps
--
-- The colors described by the 2 first ranges (0x00-0x07 and 0x08-0x0F) match exactly with
-- the 4 bit ANSI colors represented by (ColorIntensity, Color).
-- The colors of the 3rd range (0x10-0xE7) are represented by RGB8Color, and the colors of the 4th range
-- are represented by Gray8Color.
color8CodeToEither :: Color8Code
                   -> Either (ColorIntensity, Color) Color8
color8CodeToEither (Color8Code c)
  | c < 0  = error $ "negative color8 code is invalid: " ++ show c
  | c < 16 = Left $ fst $ aNSIColors !! c   -- interpreted as a 4-bit ANSI color
  | c < 232 = Right $ asRGB (c-16)          -- interpreted as 8-bit rgb
  | c < 256 = Right $ Gray8Color (c-232)    -- interpreted as 8-bit grayscale
  | otherwise = error $ "color8 code overflow :" ++ show c
 where
  asRGB i = let -- we know that i = 36 × r + 6 × g + b and (0 ≤ r, g, b ≤ 5) (cf. comment on top)
                -- so we can deduce the unique set of corresponding r g and b values:
                r = quot i 36
                g = quot (r - 36*i) 6
                b = i - (6*g + 36*i)
            in  RGB8Color r g b

-- | Finds the 4 bit color that is closest in srgb colorspace to a given 24-bit color
closest4bitsANSIColor :: SRGB.Colour Float -> (ColorIntensity, Color)
closest4bitsANSIColor color = fst $ minimumBy order aNSIColors
 where
  SRGB.RGB r g b = SRGB.toSRGB color
  order (_, c1) (_, c2) = compare (dist c1) (dist c2)
  dist c = let SRGB.RGB r' g' b' = SRGB.toSRGB c
               dr = r' - r
               dg = g' - g
               db = b' - b
           in  dr * dr + dg * dg + db * db

-- | Maps 4-bit ANSI colors to the equivalent constructors of SRGB.Colour
aNSIColors :: [((ColorIntensity, Color), SRGB.Colour Float)]
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
