module AST.Program (Vars, Program(..), Cmd(..))  where

import qualified Data.Map as Map

type Vars = Map.Map Integer Integer
data Program = Program Vars [Cmd] deriving (Show)

type Line = Integer
data Cmd = Goto Line
         | Bind Integer Integer
         | BindV Integer Integer
         | Inc Integer
         | Dec Integer
         | If Integer Integer
         | Start
         deriving (Show)