module Util
    ( unwrap
    ) where

unwrap :: Maybe a -> a
unwrap (Just it) = it
unwrap Nothing = error "tried to unwrap nothing"
