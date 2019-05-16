module Pipeline where

import Clash.Prelude


(|:>) :: (HiddenClockReset domain gated synchronous, KnownNat d)
              => DSignal domain d (Maybe a)
              -> (a -> b)
              -> DSignal domain (d+1) (Maybe b)
(|:>) signal func =
    delayed (Nothing :> Nil) $ fmap (fmap func) signal
