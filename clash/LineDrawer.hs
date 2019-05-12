module LineDrawer where

import Clash.Prelude

type Point a = (a, a)

downsample :: Bits a => Point a -> Point a
downsample point =
    let
        fn a = shiftL (shiftR a 1) 1
    in
    (fn $ fst point, fn $ snd point)

data Line a = Line { start :: Point a, end :: Point a}

type Lines a = Vec 10 (Maybe (Line a))



pixelIsOnLine :: (Eq a, Num a, Bits a) => (a, a) ->  Line a -> Bool
pixelIsOnLine pixel line =
    let
        pixel' = downsample pixel
        start' = downsample $ start line
        end' = downsample $ end line
    in
    -- Debug draw start and ends
    if pixel' == start' || pixel' == end' then
        True
    else
        False


pixelIsOnLines :: (Eq a, Num a, Bits a) => (a, a) -> Lines a -> Bool
pixelIsOnLines pixel lines =
    fold (||)
        $ fmap (maybe False (pixelIsOnLine pixel))
        $ lines






