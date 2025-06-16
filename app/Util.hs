module Util
    ( expect
    , headM
    , fst3
    , snd3
    , thd3
    ) where

expect :: String -> Maybe a -> a
expect _ (Just it) = it
expect x Nothing = error $ "tried to unwrap nothing, " ++ x

headM :: [a] -> Maybe a
headM (x:_) = Just x
headM [] = Nothing

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c
