module CompositeVideo where

import Clash.Prelude


clockFrequency = 16000000

-- 4.7us / freq
syncTime = 79

-- (10.4 - 4.7) us / freq
blankTime = 96

frameTime = 831

postFrameTime = 28



type Output = (Unsigned 5)



data Step
    = Sync (Unsigned 8)
    | Blank (Unsigned 8)
    | Frame (Unsigned 11)
    | PostFrame (Unsigned 8)
    deriving Show




frameTracker :: Step -> Step
frameTracker step =
    case step of
        Sync time ->
            if (time + 1) >= syncTime then Blank 0 else Sync (time +1)
        Blank time ->
            if (time + 1) >= blankTime then Frame 0 else Blank (time +1)
        Frame time ->
            if (time + 1) >= frameTime then PostFrame 0 else Frame (time +1)
        PostFrame time ->
            if (time + 1) >= postFrameTime then Sync 0 else PostFrame (time +1)



output :: Step -> Output
output step =
    case step of
        Sync _ -> 0
        Blank _ -> 4
        Frame _ -> 31
        PostFrame _ -> 4


behaviour :: Step -> () -> (Step, Output)
behaviour step input =
    (frameTracker step, output step)


component = mealy behaviour (Sync 0)

{-# ANN topEntity
  (Synthesize
    { t_name   = "frameTracker"
    , t_inputs = [ PortName "clk"
                 , PortName "rst"
                 ]
    , t_output = PortName "analogValue"
    }) #-}
topEntity :: Clock System Source
          -> Reset System Asynchronous
          -> Signal System ()
          -> Signal System Output
topEntity = exposeClockReset component

