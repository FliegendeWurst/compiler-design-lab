module Util
    ( unwrap
    , headM
    ) where

unwrap :: Maybe a -> a
unwrap (Just it) = it
unwrap Nothing = error "tried to unwrap nothing"

headM :: [a] -> Maybe a
headM (x:_) = Just x
headM [] = Nothing
