{-# LANGUAGE ScopedTypeVariables #-}

module LineDrawerTest where

import Clash.Prelude
import Debug.Trace
import LineDrawer


testLine :: Line 12
testLine = LineDrawer.Line (10, 10) 10 0.5 LineDrawer.XAxis

lineFunctionOnLineEdge =
    let
        stage0Input :: ( HiddenClockReset domain gated synchronous)
                    => DSignal domain 0 (Maybe (LineStageInput0 12))
        stage0Input =
            pure $ Just ((10, 10), testLine)
    in
        lineFunction stage0Input
