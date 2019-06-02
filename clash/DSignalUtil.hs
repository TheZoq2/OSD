module DSignalUtil where

import Clash.Prelude

stateMachine :: (HiddenClockReset d g s, KnownNat n)
             => a -> (a -> a) -> DSignal d n a -> DSignal d (n+1) a
stateMachine initial fn input =
    let
        internalFn input =
            (input, delayed (singleton initial) $ fmap fn input)
    in
        feedback internalFn


mealy' :: (HiddenClockReset d g s, KnownNat n)
       => a -> (a -> (a, b)) -> DSignal d n a -> DSignal d (n+1) (a, b)
mealy' initial fn input =
    let
        updateFn input =
            (input, delayed (singleton initial) $ fmap (\(i, o) -> fn i) input)
    in
        feedback updateFn
