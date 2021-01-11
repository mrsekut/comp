module ToProgram
  ( ProgramCode(..)
  , toProgramCode, toProgram
  )
  where

import AST.Program (Program(..), Vars, Cmd(..))
import qualified Data.Map as M
import qualified Code as C
import qualified Util as U


{-

Code 213797904982138037454632940231947778583451643946214351539888667374659624571846317189430034392722181574191018775500344307826886363942159348121221846668501076259189172349201939902055099238535683718239773388149828334592987613507615588

 ↓
 ↓ toProgramCode
 ↓
ProgramCode 2 2 113410085239160792121509200101
ProgramCode 2 2 <3835,437,72,33,12>
ProgramCode 2 2 <[6,2,3],[1,6],[5,2],[4,1],[1,1]>
 ↓
 ↓ toProgram, makeCmds FIXME:
 ↓
Program 4 [(If 2 3),(Goto 6),(Dec 2),(Inc 1),(Goto 1)]

-}
type InputCode = Integer
type VarsCode = Integer
type CmdsCode = Integer
data ProgramCode = ProgramCode InputCode VarsCode CmdsCode deriving (Show)


-- lengthが3であるのチェックも兼ねる
toProgramCode :: C.Code -> Either String ProgramCode
toProgramCode c = case length d of
  3 -> Right $ ProgramCode (d!!0) (d!!1) (d!!2)
  _ -> Left "not Program"
  where d = C.decode c


-- FIXME: Start
toProgram :: ProgramCode -> C.Code -> Program
toProgram (ProgramCode k m s) arg = Program (initializeVars k m arg) (Start:makeCmds s)


initializeVars :: InputCode -> VarsCode -> C.Code -> Vars
initializeVars k m arg = M.fromList $ as ++ vs
  where
    as = U.mapInd (\x i -> (toInteger i, x)) $ C.decode arg
    vs = if k == m then [] else [(v,0) | v <- [m-k..m]]


makeCmds :: CmdsCode -> [Cmd]
makeCmds c = map toCmd $ C.decode c


toCmd :: C.Code -> Cmd
toCmd c = case head d of
  1 -> checkType1 d
  2 -> checkType2 d
  3 -> checkType3 d
  4 -> checkType4 d
  5 -> checkType5 d
  6 -> checkType6 d
  _ -> error "invariant program type"
  where d = C.decode c


checkType1 :: [Integer] -> Cmd
checkType1 ix = if length ix == 2
  then Goto (ix!!1)
  else error "type error 1"


checkType2 :: [Integer] -> Cmd
checkType2 ix = if length ix == 3
  then Bind (ix!!1) (ix!!2)
  else error "type error 2"


checkType3 :: [Integer] -> Cmd
checkType3 ix = if length ix == 3
  then BindV (ix!!1) (ix!!2)
  else error "type error 3"


checkType4 :: [Integer] -> Cmd
checkType4 ix = if length ix == 2
  then Inc (ix!!1)
  else error "type error 4"


checkType5 :: [Integer] -> Cmd
checkType5 ix = if length ix == 2
  then Dec (ix!!1)
  else error "type error 5"


checkType6 :: [Integer] -> Cmd
checkType6 ix = if length ix == 3
  then If (ix!!1) (ix!!2)
  else error "type error 6"

