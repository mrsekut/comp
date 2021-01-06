module Code (Code, element, code, decode, len, replace, sequence, pair) where

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


-- FIXME: clean, performance
depair :: Code -> (Integer, Integer)
depair 0 = (0, 0)
depair n = (left, right)
  where
    ans = head $ filter ((>= n) . fst) candi
    right = fst ans - n
    left = snd ans - right


candi :: [(Code, Integer)]
candi = [(x, i) | (x,i) <- zip cand [0..]]


cand :: [Code]
cand = [sum0toN (k+1) | k <- [0..]]


sum0toN :: Integer -> Integer
sum0toN n = div (n * (n+1)) 2