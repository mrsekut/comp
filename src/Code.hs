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



-- FIXME: clean, fix: map depair [1..500]などで負数が出るところに着目し、depairもしくはsearchを修正する
depair :: Code -> (Integer, Integer)
depair 1 = (0, 0)
depair n = (left, right)
  where
    max = ceiling $ fromIntegral (1 + binLength n) / 2
    (a, b) = search (2^(max-1)) (2^max) n
    right = b - n
    left = a - right


binLength :: Integer -> Integer
binLength = round . logBase 2 . fromIntegral


search :: Integer -> Integer  -> Integer -> (Integer, Integer)
search n1 n2 n
  | n2 - n1 == 1 = (n2, f n2)
  | n < mid      = search n1 midI n
  | n > mid      = search midI n2 n
  | otherwise    = (midI, mid)
  where
    -- FIXME: clean, name
    midI = div (n1 + n2) 2
    mid = f midI
    f n = div ((n+1) * (n+2)) 2


candi :: [(Code, Integer)]
candi = [(x, i) | (x,i) <- zip cand [0..]]


cand :: [Code]
cand = [sum0toN (k+1) | k <- [0..]]


sum0toN :: Integer -> Integer
sum0toN n = div (n * (n+1)) 2
