module Comp (execute) where

import Program (Program(..), Vars, Cmd(..))
import qualified Data.Map as Map
import Data.Maybe (fromJust)


-- 標準入力など
-- main :: IO ()
-- main = undefined

comp :: Integer -> Integer -> Integer
comp = undefined

-- initialize :: Program -> Vars
-- initialize (ProgramCode _ m _) = Map.fromList (map (,0) [1..m])


execute :: Program -> Integer
execute (Program vs cmds) = execute' cmds 1 vs


-- FIXME: clean
execute' :: [Cmd] -> Integer -> Vars -> Integer
execute' [] _ _ = undefined
execute' cmd pc vs =
    if pc == toInteger(length cmd) -- NOTE: Startがいるので。いないなら+1
    then fromJust $ Map.lookup 1 vs
    else execute' cmd next nvs
        where (next, nvs) = row (cmd !! fromInteger pc, vs) pc



-- FIXME: clean
row :: (Cmd, Vars) -> Integer -> (Integer, Vars)
row (Goto l, vs) _     = (l, vs)
row (Bind v n, vs) pc  = (pc+1, Map.insert v n vs)
row (BindV a b, vs) pc = (pc+1, bind a b vs)
row (Inc a, vs) pc     = (pc+1, Map.update inc a vs)
row (Dec a, vs) pc     = (pc+1, Map.update dec a vs)
row (If n l, vs) pc    = case Map.lookup n vs of
    Just x -> if x>0 then (l, vs) else (pc+1, vs)
    Nothing -> error "wwwww"

inc :: Num a => a -> Maybe a
inc n = Just (n+1)

dec :: (Ord a, Num a) => a -> Maybe a
dec n = if n > 0 then Just (n-1) else Just n

bind :: Ord a => a -> a -> Map.Map a a -> Map.Map a a
bind a b vs = Map.insert (fromJust $ Map.lookup b vs) a vs