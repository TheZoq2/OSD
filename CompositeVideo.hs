module CompositeVideo where

import Clash.Prelude

import qualified LineDrawer


testLines ::LineDrawer.Lines Coordinate
testLines =
    (  Just (LineDrawer.Line (100, 100) (200, 200))
    :> Just (LineDrawer.Line (100, 200) (200, 100))
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




type Output = (Unsigned 5)
type Coordinate = (Unsigned 11)


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

drawLines :: LineDrawer.Lines Coordinate -> Coordinate -> Coordinate -> Output
drawLines lines x y =
    if LineDrawer.pixelIsOnLines (x, y) lines then
        white
    else
        black

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







-- Putting things together

output :: (Coordinate -> Coordinate -> Output) -> HStep -> Coordinate ->  Output
output pixelFunction step line =
    case step of
        Sync _ -> 0
        Blank _ -> syncLevel
        Frame pixel -> pixelFunction pixel line
        PostFrame _ -> syncLevel


vsyncOutput :: VStep -> Output
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
            vsyncOutput step
        Drawing line step ->
            output (drawLines testLines) step line


behaviour :: FullFrameStep -> () -> (FullFrameStep, (Output, Bit))
behaviour step input =
    let
        debugOutput =
            case step of
                VerticalSync _ _ -> 1
                _ -> 0
    in
    ((fullFrame step), (fullOutput step, debugOutput))

behaviourVsync :: (Bool, VStep) -> () -> ((Bool, VStep), (Output, Bit))
behaviourVsync (even, step) input =
    let
        (newEven, newState) =
            if step /= Done then
                (even, vsync even step)
            else
                (not even, PreEqualising 0 0)

        isNewFrame = step == Done && even == False
    in
        ((newEven, newState), (vsyncOutput step, unpack $ pack $ even))


-- component = mealy behaviourVsync (False, HalfFrame 0)
component = mealy behaviour (Drawing 0 (Sync 0))

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
          -> Signal System (Output, Bit)
topEntity = exposeClockReset component

