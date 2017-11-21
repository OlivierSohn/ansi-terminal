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
{-# INLINE xtermMapRGB8bitComponent #-}
xtermMapRGB8bitComponent :: Int -> Int
xtermMapRGB8bitComponent 0 = 0
xtermMapRGB8bitComponent n = 55 + n * 40

-- | how xterm interprets 8bit grayscale colors (deduced from https://jonasjacek.github.io/colors/)
{-# INLINE xtermMapGray8bitComponent #-}
xtermMapGray8bitComponent :: Int -> Int
xtermMapGray8bitComponent v = 8 + 10 * v

{-# INLINE clamp #-}
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

color8CodeToEither :: Color8Code -> Either (ColorIntensity, Color) Color8
color8CodeToEither (Color8Code c)
  | c < 0  = error $ "negative color8 code is invalid: " ++ show c
  | c < 16 = Left $ fst $ aNSIColors !! c
  | c < 232 = Right $ asRGB (c-16)
  | c < 256 = Right $ Gray8Color (c-232)
  | otherwise = error $ "color8 code overflow :" ++ show c
 where
  asRGB i = let r = quot i 36
                g = quot (r - 36*i) 6
                b = i - (6*g + 36*i)
            in  RGB8Color r g b

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
