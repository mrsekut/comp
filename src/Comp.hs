module Comp where



-- 標準入力など
main :: IO ()
main = undefined

type Code = Integer
comp :: Code -> Code -> Either Integer String
comp = undefined





-- isProgramCode




-- コード化


-- FIXME: 負数は弾く必要がある

code :: [Integer] -> Code
code = foldr pair 0

pair :: Integer -> Integer -> Code
pair x y = sum [0..x+y] + x + 1



-- コードのUtils

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


element' :: Code -> Integer -> Integer
element' = undefined
-- element' xs i = (decode xs) !! i-1

codeLength :: Code -> Integer
-- length' = length . decode
codeLength = undefined

replace' :: Code -> Integer -> Integer
replace' = undefined
-- replace' xs i newElement = take (i-1) (decode xs) ++ [newElement] ++ drop i (decode xs)

sequence' :: Integer -> Integer -> Code
sequence' = undefined
-- sequence' = flip replicate
