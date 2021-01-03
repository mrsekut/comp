module Util ( mapInd ) where


mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [1..]