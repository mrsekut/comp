module AST.Program (Vars, Program(..), Cmd(..))  where

import qualified Data.Map as M

type Vars = M.Map Integer Integer
data Program = Program Vars [Cmd] deriving (Show)

type Line = Integer
data Cmd = Goto Line                -- goto La
         | Bind Integer Integer     -- va = b
         | BindV Integer Integer    -- va = vb
         | Inc Integer              -- va++
         | Dec Integer              -- va--'
         | If Integer Integer       -- if (va > 0) goto Lb
         | Start
         deriving (Show)