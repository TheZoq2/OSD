{-# LANGUAGE ScopedTypeVariables #-}

module LineDrawer where

import Clash.Prelude
import Debug.Trace


-- Extends a signed integer to a signed fixpoint value
signedToFp :: forall n f. (KnownNat n, KnownNat f) => Signed n -> SFixed n f
signedToFp signed =
    let
        extended :: Signed (n+f)
        extended = extend signed 

        amountToShift :: Int
        amountToShift = (fromInteger $ snatToInteger (SNat :: SNat f))

        shiftedInt :: Signed (n+f)
        shiftedInt = shiftL extended amountToShift
    in
        sf (SNat :: SNat f) shiftedInt


-- Truncates the fractional part from a fixpoint value
truncateFp :: forall n f. (KnownNat n, KnownNat f) => SFixed n f -> Signed n
truncateFp fixed =
    let
        rawInt = unSF fixed

        amountToShift :: Int
        amountToShift = (fromInteger $ snatToInteger (SNat :: SNat f))

        shiftedInt :: Signed (n+f)
        shiftedInt = shiftR rawInt amountToShift
    in
        truncateB shiftedInt



-- Type representing a single point on the screen
type Point n = (Signed n, Signed n)

pointX :: Point n -> Signed n
pointX = fst

pointY :: Point n -> Signed n
pointY = snd



data Axis = XAxis | YAxis
type Slope = SFixed 2 6

{-
  Type representing a line. Slope must be between -1 and 1.

  Drawing is based on the standard line equation y=kx+m.

  If axis = YAxis then all coordinates are swaped and x=ky+m is used instead.
  This allows drawing lines in all directions without slopes being too large
-}
data Line n = Line
    -- The starting point of the line. Must be smaller than n on the specified
    -- axis
    { start :: Point n
    -- The length of the line on the specified axis
    , end :: Point n
    -- The slope of the line as described above. -1 <= slope <= 1
    , slope :: Slope
    , axis :: Axis
    }

-- A collection of lines that are checked in parallel. If less than the maximum
-- amount of lines should be drawn, use `Nothing`
type Lines n = Vec 31 (Maybe (Line n))



-- Calculates the secondary axis coordinate of the specified line given
-- the primary coordinate relative to the start of the line
lineY :: forall n. KnownNat n => Line n -> Signed n -> Signed n
lineY line xCoordRelative =
    let
        slopeEnlarged :: SFixed n 6
        slopeEnlarged = (resizeF $ slope line)

        xCoordFixed :: SFixed n 6
        xCoordFixed = signedToFp xCoordRelative

        relativeY = (truncateFp (xCoordFixed * slopeEnlarged))
    in
        pointY (start line) + relativeY


maybeSwapAxes :: (Signed n, Signed n) -> Line n -> (Signed n, Signed n)
maybeSwapAxes (x, y) line =
    case axis line of
        XAxis -> (x, y)
        YAxis -> (y, x)


-- Checks if the specified coordinate is on the specified line
pixelIsOnLine :: forall n. KnownNat n => (Signed n, Signed n) -> Line n -> Bool
pixelIsOnLine pixel line =
    let
        pixel' = maybeSwapAxes pixel line
        -- pixel' = pixel

        start' = start line
        end' = end line

        xRelative = pointX pixel' - pointX start'

        expectedY = lineY line xRelative

        valid = (pointX pixel' >= pointX start') && (pointX pixel' <= pointX end')
    in
        (abs $ (pointY pixel') - expectedY) < 2 && valid



-- Check if the specified pixel is on any of the specified lines
pixelIsOnLines :: KnownNat n  => (Signed n, Signed n) -> Lines n -> Bool
pixelIsOnLines pixel lines =
    fold (||)
        $ fmap (maybe False (pixelIsOnLine pixel))
        $ lines






