module Comp (main, execute) where
import AST.Program (Program(..), Vars, Cmd(..))
import qualified Data.Map as M
import qualified Code as C
import qualified ToProgram as T
import Data.Maybe (fromJust)


main :: IO ()
main = do
  let tashizan = 213797904982138037454632940231947778583451643946214351539888667374659624571846317189430034392722181574191018775500344307826886363942159348121221846668501076259189172349201939902055099238535683718239773388149828334592987613507615588
  let arg = C.code [50, 4]
  print $ comp tashizan arg


-- FIXME: use Faillible
comp :: Integer -> Integer -> Integer
comp p x = execute $ T.toProgram p' x
  where
    p' = either error id $ T.toProgramCode p


execute :: Program -> Integer
execute (Program vs cmds) = execute' cmds 1 vs


-- FIXME: clean
execute' :: [Cmd] -> Integer -> Vars -> Integer
execute' [] _ _ = undefined
execute' cmd pc vs =
  if pc == toInteger(length cmd) -- NOTE: Startがいるので。いないなら+1
  then fromJust $ M.lookup 1 vs
  else execute' cmd next nvs
    where (next, nvs) = row (cmd !! fromInteger pc, vs) pc


-- FIXME: clean
row :: (Cmd, Vars) -> Integer -> (Integer, Vars)
row (Goto l, vs)    _  = (l, vs)
row (Bind v n, vs)  pc = (pc+1, M.insert v n vs)
row (BindV a b, vs) pc = (pc+1, bind a b vs)
row (Inc a, vs)     pc = (pc+1, M.update inc a vs)
row (Dec a, vs)     pc = (pc+1, M.update dec a vs)
row (If n l, vs)    pc = case M.lookup n vs of
  Just x -> if x>0 then (l, vs) else (pc+1, vs)
  Nothing -> error "error"


inc :: Num a => a -> Maybe a
inc n = Just (n+1)


dec :: (Ord a, Num a) => a -> Maybe a
dec n = if n > 0 then Just (n-1) else Just n


bind :: Ord a => a -> a -> M.Map a a -> M.Map a a
bind a b vs = M.insert (fromJust $ M.lookup b vs) a vs