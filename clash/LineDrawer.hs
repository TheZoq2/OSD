{-# LANGUAGE ScopedTypeVariables #-}

module LineDrawer where

import Clash.Prelude
import Debug.Trace
import qualified Data.List as List


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
    , lengthAlongAxis :: Signed n
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


data SwapedPoint n = SwapedPoint (Point n)

maybeSwapAxes :: KnownNat n => Line n -> Point n -> SwapedPoint n
maybeSwapAxes line (x, y) =
    SwapedPoint $ case axis line of
        XAxis -> (x, y)
        YAxis -> (y, x)


calculateLineY :: KnownNat n => Line n -> SwapedPoint n -> Signed n
calculateLineY line (SwapedPoint pixel) =
    let
        start' = start line
        end' = pointX start' + lengthAlongAxis line

        xRelative = pointX pixel - pointX start'

    in
        lineY line xRelative

-- Checks if the specified coordinate is on the specified line
pixelIsOnLine :: KnownNat n => SwapedPoint n -> Line n -> (Signed n) -> Bool
pixelIsOnLine (SwapedPoint pixel) line expectedY =
    let
        start' = start line
        end' = lengthAlongAxis line
        valid = (pointX pixel >= pointX start') && (pointX pixel <= end')
    in
        (abs $ (pointY pixel) - expectedY) < 0 && valid




-- pixelIsOnLinesD :: KnownNat n => Signal ()

-- Check if the specified pixel is on any of the specified lines
pixelIsOnLines :: KnownNat n  => Point n -> Lines n -> Bool
pixelIsOnLines pixel lines =
    let
        lineFunction line =
            let
                swaped = (maybeSwapAxes line pixel)
                expectedY = calculateLineY line swaped
            in
                pixelIsOnLine swaped line expectedY
    in
    fold (||)
        $ fmap (maybe False lineFunction)
        $ lines




-- Debug functions

debugLines :: LineDrawer.Lines 10
debugLines =
    (  Just (LineDrawer.Line (10, 10) 10 0.5 LineDrawer.XAxis)
    :> Just (LineDrawer.Line (10, 14) 10 (0.5) LineDrawer.XAxis)
    :> Just (LineDrawer.Line (10, 14) 10 (-0.5) LineDrawer.XAxis)
    :> Just (LineDrawer.Line (25, 18) 10 (-1.0) LineDrawer.XAxis)
    :> Nil
    )
    ++ repeat Nothing

debugLinesToStrings :: Lines 10 -> [String]
debugLinesToStrings lines =
    let
        -- Bild a list of nothing representing the pixels we are going
        -- to output
        xDim = 80
        yDim = 30

        xFn :: Signed 10 -> Signed 10 -> Bool
        xFn y x = pixelIsOnLines (x, y) lines

        yFn :: (Signed 10) -> [Bool]
        yFn y = fmap (xFn y) $ List.take xDim [0..]
    in
        List.map
            (\x -> List.map (\a -> if a then '*' else ' ') x)
            $ List.take yDim
            $ List.map yFn [0..]


debugDrawLines :: Lines 10 -> IO [()]
debugDrawLines lines =
    mapM putStrLn $ debugLinesToStrings lines
