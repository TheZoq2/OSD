{-# LANGUAGE ScopedTypeVariables #-}

module CompositeVideo where

import Clash.Prelude

import qualified LineDrawer
import qualified DSignalUtil


testLines :: LineDrawer.Lines 11
testLines =
    (  Just (LineDrawer.Line (100, 100) 100 0.5 LineDrawer.XAxis)
    :> Just (LineDrawer.Line (100, 110) 100 (0.5) LineDrawer.XAxis)
    :> Just (LineDrawer.Line (100, 110) 100 (-0.5) LineDrawer.XAxis)
    :> Just (LineDrawer.Line (250, 120) 150 (-0.9) LineDrawer.XAxis)
    :> Nil
    )
    ++ repeat Nothing



-- Horizontal timings (Rely on 16MHz clock)
syncTime = 78
blankTime = 96
frameTime = 831
postFrameTime = 28
frameMidTime = 512 - syncTime - blankTime

-- Vertical timings (Rely on 16MHz clock)
halfFrameTime = 512
fullFrameTime = 1024
equalisingPulseTime = 38
fieldSeparationTime = 75

preEqualisingCount = 5
equalisingCount = 5
postEqualisingCount = 5


-- Voltages
syncLevel = 6
white = 31
black = 8




type Coordinate = (Unsigned 11)
type PixelValue = (Unsigned 5)

data Output
    = OutputSync (Unsigned 5)
    | Pixel (Coordinate, Coordinate)


-- TODO: Select appropriate word lengths
data VStep
    -- The nth pre equalising pulse which has been active for k clocks
    = PreEqualising (Unsigned 8) (Unsigned 12)
    | FieldSync (Unsigned 8) (Unsigned 12)
    | PostEqualising (Unsigned 8) (Unsigned 12)
    | HalfEmptyFrame (Unsigned 12)
    | EmptyFrame (Unsigned 8) (Unsigned 12)
    | HalfFrame (Unsigned 12)
    | Done
    deriving(Eq, Show)


nextVStep :: Bool -> VStep -> VStep
nextVStep _     (PreEqualising 4 _)      = FieldSync      0         0
nextVStep _     (PreEqualising count _)  = PreEqualising  (count+1) 0
nextVStep _     (FieldSync 4 _)          = PostEqualising 0         0
nextVStep _     (FieldSync count _)      = FieldSync      (count+1) 0
nextVStep True  (PostEqualising 4 _)     = EmptyFrame    0         0
nextVStep False (PostEqualising 4 _)     = HalfEmptyFrame 0
nextVStep _     (PostEqualising count _) = PostEqualising (count+1) 0
nextVStep _     (HalfEmptyFrame _)       = EmptyFrame    0         0
nextVStep True  (EmptyFrame 16 _)        = HalfFrame     0
nextVStep False (EmptyFrame 16 _)       = Done
nextVStep _     (EmptyFrame count _)     = EmptyFrame    (count+1) 0
nextVStep _     (HalfFrame _)            = Done
nextVStep _     Done                     = Done


incrementVStepTime :: VStep -> VStep
incrementVStepTime step =
    case step of
        PreEqualising c time -> PreEqualising c (time+1)
        FieldSync c time -> FieldSync c (time+1)
        PostEqualising c time -> PostEqualising c (time+1)
        HalfEmptyFrame time -> HalfEmptyFrame (time+1)
        EmptyFrame c time -> EmptyFrame c (time+1)
        HalfFrame time -> HalfFrame (time+1)
        Done -> Done



vsync :: Bool -> VStep -> VStep
vsync evenField step =
    let
        nextStep = nextVStep evenField step
    in
    case step of
        PreEqualising c time ->
            if (time+1) >= halfFrameTime then nextStep else incrementVStepTime step
        FieldSync c time ->
            if (time+1) >= halfFrameTime then nextStep else incrementVStepTime step
        PostEqualising c time ->
            if (time+1) >= halfFrameTime then nextStep else incrementVStepTime step
        HalfEmptyFrame time ->
            if (time+1) >= halfFrameTime then nextStep else incrementVStepTime step
        EmptyFrame c time ->
            if (time+1) >= fullFrameTime then nextStep else incrementVStepTime step
        HalfFrame time ->
            if (time+1) >= halfFrameTime then nextStep else incrementVStepTime step
        Done -> Done



data HStep
    = Sync (Unsigned 8)
    | Blank (Unsigned 8)
    | Frame Coordinate
    | PostFrame (Unsigned 8)
    | HDone
    deriving Show


horizontalLineGenerator :: HStep -> HStep
horizontalLineGenerator step =
    case step of
        Sync time ->
            if (time + 1) >= syncTime then Blank 0 else Sync (time +1)
        Blank time ->
            if (time + 1) >= blankTime then Frame 0 else Blank (time +1)
        Frame time ->
            if (time + 1) >= frameTime then PostFrame 0 else Frame (time +1)
        PostFrame time ->
            if (time + 1) >= postFrameTime then HDone else PostFrame (time +1)
        HDone -> Sync 0




-- Drawing functions

drawLines :: LineDrawer.Lines 12 -> Coordinate -> Coordinate -> PixelValue
drawLines lines x y =
    let
        xCoord = (fromInteger $ toInteger x) `shiftR` 0
        yCoord = (fromInteger $ toInteger y) `shiftR` 0
    in
    if LineDrawer.pixelIsOnLines (xCoord, yCoord) lines then
        white
    else
        black

{-
stepPixel :: Coordinate -> Coordinate -> Output
stepPixel x y =
    if y == 300 || y == 301 || x == 300 || x == 301 then
        -- 2 + syncLevel + (truncateB $ shiftR x 6)
        white
    else
        2 + syncLevel + (truncateB $ shiftR x 7)
    -- if x > 300 then
    --     2 + syncLevel + (truncateB $ shiftR x 6)
    --     -- 20
    -- else
    --     white
-}







-- Putting things together

output :: HStep -> Coordinate -> Output
output step line =
    case step of
        Sync _ -> OutputSync 0
        Blank _ -> OutputSync syncLevel
        Frame pixel -> Pixel (pixel, line)
        PostFrame _ -> OutputSync syncLevel


vsyncOutput :: VStep -> PixelValue
vsyncOutput step =
    case step of
        PreEqualising _ time ->
            if time < equalisingPulseTime then 0 else syncLevel
        FieldSync _ time ->
            if time > (halfFrameTime - fieldSeparationTime) then syncLevel else 0
        PostEqualising _ time ->
            if time < equalisingPulseTime then 0 else syncLevel
        HalfEmptyFrame _ ->
            syncLevel
        EmptyFrame _ time ->
            if time < syncTime then 0 else syncLevel
        HalfFrame time ->
            if time < syncTime then 0 else syncLevel
        Done ->
            0


data FullFrameStep
    = VerticalSync Bool VStep
    | Drawing Coordinate HStep
    deriving(Show)


fullFrame :: FullFrameStep -> FullFrameStep
fullFrame step =
    case step of
        VerticalSync even Done ->
            if even then Drawing 0 (Frame frameMidTime) else Drawing 1 (Sync 0)
        VerticalSync even step ->
            VerticalSync even $ vsync even step
        Drawing line step ->
            let
                newLine =
                    case step of
                        HDone -> line+2
                        _ -> line
            in
            case (line, step) of
                (576, _) -> VerticalSync False (PreEqualising 0 0)
                (575, Frame time) ->
                    if time == frameMidTime then
                        VerticalSync True (PreEqualising 0 0)
                    else
                        Drawing newLine (horizontalLineGenerator step)
                _ ->
                    Drawing newLine (horizontalLineGenerator step)


fullOutput :: FullFrameStep -> Output
fullOutput step =
    case step of
        VerticalSync _ step ->
            OutputSync $ vsyncOutput step
        Drawing line step ->
            output step line


behaviour :: FullFrameStep -> () -> (FullFrameStep, (Output, Bit))
behaviour step input =
    let
        debugOutput =
            case step of
                VerticalSync _ _ -> 1
                _ -> 0
    in
    ((fullFrame step), (fullOutput step, debugOutput))


-- component = mealy behaviourVsync (False, HalfFrame 0)
syncHandler :: HiddenClockReset domain gated synchronous
          => Signal domain ()
          -> Signal domain (Output, Bit)
syncHandler = mealy behaviour (Drawing 0 (Sync 0))


lineDrawer :: forall domain g s. (HiddenClockReset domain g s)
           => DSignal domain 0 (Output, Bit)
           -> DSignal domain 3 (Maybe (PixelValue, Bit))
lineDrawer input =
    let
        coordToPoint :: (Coordinate, Coordinate) -> LineDrawer.Point 11
        coordToPoint (x, y) = (unpack $ pack x, unpack $ pack y)

        lineDrawerInput = fmap (\(state, _) -> case state of
                          Pixel coord -> coordToPoint coord
                          _ -> (0, 0)
                     )
                     input

        pixelOn = LineDrawer.pixelIsOnLinesD lineDrawerInput (pure testLines)
        pixelValue :: DSignal domain 3 (Unsigned 5)
        pixelValue = fmap (\x -> if x then white else black) pixelOn

        combiner :: Maybe (Output, Bit) -> (Unsigned 5) -> Maybe (PixelValue, Bit)
        combiner input pixel =
            case input of
                Just (Pixel _, dbg) -> Just (pixel, dbg)
                Just (OutputSync level, dbg) -> Just (level, dbg)
                Nothing -> Nothing
    in
        pure combiner <*> (delayedI $ fmap Just input) <*> pixelValue



fullComponent :: HiddenClockReset domain gated synchronous
          => Signal domain ()
          -> DSignal domain 3 (Maybe (PixelValue, Bit))
fullComponent input =
    lineDrawer $ pure (OutputSync 0, 0)
    -- lineDrawer $ unsafeFromSignal $ syncHandler input



{-# ANN topEntity
  (Synthesize
    { t_name   = "frameTracker"
    , t_inputs = [ PortName "clk"
                 , PortName "rst"
                 ]
    , t_output = PortProduct "" 
        [ PortName "analogValue"
        , PortName "newFrame"
        ]
    }) #-}
topEntity :: Clock System Source
          -> Reset System Asynchronous
          -> Signal System ()
          -> DSignal System 3 (Maybe (PixelValue, Bit))
topEntity = exposeClockReset fullComponent

