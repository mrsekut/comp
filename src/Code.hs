module Code (Code, element, code, decode, len, replace, sequence) where

import Prelude hiding (sequence)

-- コード化

type Code = Integer

-- FIXME: 負数は弾く必要がある

code :: [Integer] -> Code
code = foldr pair 0

pair :: Integer -> Integer -> Code
pair x y = sum [0..x+y] + x + 1



-- コードのUtils

-- TODO: パフォーマンス的な改善
depair :: Code -> (Integer, Integer)
depair 0 = (0, 0)
depair c = head $ filter (\x -> uncurry pair x == c) candidates
    where
        candidates = [(x,y) | x <- [0..c], y <- [0..c]]

left :: Code -> Integer
left = fst . depair

right :: Code -> Integer
right = snd . depair

decode :: Code -> [Integer]
decode 0 = []
decode c = left c : decode (right c)


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