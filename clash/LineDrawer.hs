{-# LANGUAGE ScopedTypeVariables #-}

module LineDrawer where

import Clash.Prelude
import Debug.Trace
import qualified Data.List as List
import Pipeline


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



data Axis = XAxis | YAxis deriving Show
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
    } deriving Show

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


data SwapedPoint n = SwapedPoint (Point n) deriving Show

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
        valid = (pointX pixel >= pointX start') && (pointX pixel <= end' + pointX start')
    in
        (abs $ (pointY pixel) - expectedY) < 2 && valid



type LineStageInput0 n = (Point n, Line n)
type LineStageInput1 n = (SwapedPoint n, Line n)
type LineStageInput2 n = (SwapedPoint n, Signed n, Line n)

lineFunction :: forall d0 domain gated synchronous n.
           ( HiddenClockReset domain gated synchronous
           , KnownNat d0
           , KnownNat n
           )
        => DSignal domain d0 (Maybe (LineStageInput0 n))
        -> DSignal domain (d0+3) (Maybe Bool)
lineFunction input =
    let
        stage0 (pixel, line) = (maybeSwapAxes line pixel, line)

        stage1 (swaped, line) =
            (swaped, line, calculateLineY line swaped)

        stage2 (swaped, line, expectedY) = pixelIsOnLine swaped line expectedY
    in
        input |:> stage0 |:> stage1 |:> stage2


pixelIsOnLinesD :: forall d0 d1 domain gated synchronous n.
                   ( HiddenClockReset domain gated synchronous
                   , KnownNat d0
                   , KnownNat n
                   )
                => DSignal domain d0 (Point n)
                -> DSignal domain d0 (Lines n)
                -> DSignal domain (d0+3) Bool
pixelIsOnLinesD pixel lines =
    let
        individualLines = sequenceA lines

        maybeJoin :: a -> Maybe b -> Maybe (a, b)
        maybeJoin lhs Nothing = Nothing
        maybeJoin lhs (Just rhs) = Just (lhs, rhs)

        lineFnWithPixel :: DSignal domain d0 (Maybe (Line n))
                        -> DSignal domain (d0+3) (Maybe Bool)
        lineFnWithPixel line =
            lineFunction $ liftA2 maybeJoin pixel line

        perLine = fmap lineFnWithPixel individualLines

        bools = sequenceA perLine
    in
        fmap (foldl1 (||)) $ fmap (fmap (maybe False id)) bools

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
