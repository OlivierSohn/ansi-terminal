-- | Types used to represent SELECT GRAPHIC RENDITION (SGR) aspects.
module System.Console.ANSI.Types
  (
    SGR (..)
  , ConsoleLayer (..)
  -- | ANSI colors come in different flavors (3/4 bits, 8bits, 24 bits) :
  --   https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
  , Color (..)
  , Color8(..)
  , Color8Code(..)
  , ColorIntensity (..)
  , ConsoleIntensity (..)
  , Underlining (..)
  , BlinkSpeed (..)
  ) where

import Data.Ix (Ix)

import Data.Colour (Colour)

-- | ANSI 4-bit colors: come in various intensities, which are controlled by
-- 'ColorIntensity'
data Color = Black
           | Red
           | Green
           | Yellow
           | Blue
           | Magenta
           | Cyan
           | White
           deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI 4-bit colors come in two intensities
data ColorIntensity = Dull
                    | Vivid
                    deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)


newtype Color8Code = Color8Code Int deriving (Eq, Show, Read)

-- | Represents 8-bit ANSI colors whose codes are in the range 0x10 - 0xFF.
--
--  For reference, here are all ranges of 8-bit ANSI colors as defined in
--  https://en.wikipedia.org/wiki/ANSI_escape_code#Colors lists the ranges:
--
-- 0x00-0x07:  standard colors (as in ESC [ 30–37 m)
-- 0x08-0x0F:  high intensity colors (as in ESC [ 90–97 m)
-- 0x10-0xE7:  6 × 6 × 6 cube (216 colors): 16 + 36 × r + 6 × g + b (0 ≤ r, g, b ≤ 5)
-- 0xE8-0xFF:  grayscale from black to white in 24 steps
--
--  The 8-bit ANSI colors whose codes are in the 0x00-0x0F range are equivalent to the
--  ANSI 4-bit colors, hence they are not represented by Color8, but by
--  the pair (ColorIntensity, Color).
--
data Color8 = RGB8Color !Int !Int !Int -- ^ ANSI 8-bit "6 × 6 × 6 cube" (216 colors) rgb.
                                       --   r,g,b components are in range [0..5].
            | Gray8Color !Int          -- ^ ANSI 8-bit "grayscale" colors.
                                       --   gray component is in range [0..23].
            deriving (Eq, Show, Read)

-- | ANSI colors can be set on two different layers
data ConsoleLayer = Foreground
                  | Background
                  deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI blink speeds: values other than 'NoBlink' are not widely supported
data BlinkSpeed = SlowBlink -- ^ Less than 150 blinks per minute
                | RapidBlink -- ^ More than 150 blinks per minute
                | NoBlink
                deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI text underlining
data Underlining = SingleUnderline
                 | DoubleUnderline -- ^ Not widely supported
                 | NoUnderline
                 deriving (Eq, Ord, Bounded ,Enum, Show, Read, Ix)

-- | ANSI general console intensity: usually treated as setting the font style
-- (e.g. 'BoldIntensity' causes text to be bold)
data ConsoleIntensity = BoldIntensity
                      | FaintIntensity -- ^ Not widely supported: sometimes
                                       -- treated as concealing text
                      | NormalIntensity
                      deriving (Eq, Ord, Bounded, Enum, Show, Read, Ix)

-- | ANSI Select Graphic Rendition command
data SGR = Reset
         | SetConsoleIntensity ConsoleIntensity
         | SetItalicized Bool -- ^ Not widely supported: sometimes treated as
                              -- swapping foreground and background
         | SetUnderlining Underlining
         | SetBlinkSpeed BlinkSpeed
         | SetVisible Bool -- ^ Not widely supported
         | SetSwapForegroundBackground Bool
         | SetColor ConsoleLayer ColorIntensity Color
         | SetColor8 ConsoleLayer Color8
         | SetColor8Code ConsoleLayer Color8Code
         | SetRGBColor ConsoleLayer (Colour Float) -- ^ Supported from Windows 10
                                                   -- Creators Update
         deriving (Eq, Show, Read)
