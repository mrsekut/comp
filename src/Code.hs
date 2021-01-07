module Code
  ( Code
  , element, code, decode, len, replace, sequence
  , pair, depair, search
  ) where

import Prelude hiding (sequence)



-- Code


type Code = Integer


-- FIXME: 負数は弾く必要がある
code :: [Integer] -> Code
code = foldr pair 0


decode :: Code -> [Integer]
decode 0 = []
decode c = l : decode r
  where (l, r) = depair c


pair :: Integer -> Integer -> Code
pair x y = sum0toN (x+y)  + x + 1


left, right :: Code -> Integer
left = fst . depair
right = snd . depair


-- indexは1始まり
element :: Code -> Int -> Integer
element c i = if i <= length d then d !! (i-1) else error "index too large in element"
  where d = decode c


len :: Code -> Int
len = length . decode


replace :: Code -> Int -> Integer -> Code
replace xs i n = code $ take (i-1) (decode xs) ++ [n] ++ drop i (decode xs)


sequence :: Integer -> Int -> Code
sequence c n = code $ replicate n c



-- Utils


depair :: Code -> (Integer, Integer)
depair 1 = (0, 0)
depair 2 = (0, 1)
depair n = (left, right)
  where
    pow = ceiling $ fromIntegral (1 + binLength n) / 2
    (min, max) = rangeCheck n pow
    (a, b) = search min max n
    right = b - n
    left = a - right


rangeCheck :: Integer -> Integer -> (Integer, Integer)
rangeCheck n pow
  | n < leftNum (2^(pow-1)) = (2^(pow-2), 2^(pow-1))
  | n > leftNum (2^pow)     = (2^pow, 2^(pow+1))
  | otherwise               = (2^(pow-1), 2^pow)


binLength :: Integer -> Integer
binLength = (+1) . floor . logBase 2 . fromIntegral


search :: Integer -> Integer  -> Integer -> (Integer, Integer)
search n1 n2 n
  | leftNum n1 == n = (n1, leftNum n1)
  | leftNum n2 == n = (n2, leftNum n2)
  | n2 - n1 == 1    = (n2, leftNum n2)
  | n < mid         = search n1 midI n
  | n > mid         = search midI n2 n
  | otherwise       = (midI, mid)
  where
    midI = div (n1 + n2) 2
    mid = leftNum midI


leftNum :: Integral a => a -> a
leftNum n = div ((n+1) * (n+2)) 2

candi :: [(Code, Integer)]
candi = [(x, i) | (x,i) <- zip cand [0..]]


cand :: [Code]
cand = [sum0toN (k+1) | k <- [0..]]


sum0toN :: Integer -> Integer
sum0toN n = div (n * (n+1)) 2
