module ToProgram where

import AST.Program (Program(..), Vars, Cmd(..))
import qualified Data.Map as M
import qualified Code as C


type Result a = Either [String] a




{-

Code 213797904982138037454632940231947778583451643946214351540021648522868699116020532367794717279183597514183155623196270804266008911831504391186653218399859236746627965664600576149708185774554408383297605366764401745745512661359368506
 ↓
 ↓ toProgramCode
 ↓
ProgramCode 2 3 113410085239160792121509200101
ProgramCode 2 3 <3835,437,72,33,12>
ProgramCode 2 3 <[6,2,3],[1,6],[5,2],[4,1],[1,1]>
 ↓
 ↓ toProgram, makeCmds
 ↓
Program 4 [(If 2 3),(Goto 6),(Dec 2),(Inc 1),(Goto 1)]

-}
type InputCode = Integer
type VarsCode = Integer
type CmdsCode = Integer
data ProgramCode = ProgramCode InputCode VarsCode CmdsCode deriving (Show)


-- FIXME: unsafe, Result
-- lengthが3であるのチェックも兼ねる
toProgramCode :: C.Code -> Result ProgramCode
toProgramCode c = case length d of
  3 -> Right $ ProgramCode (d!!0) (d!!1) (d!!2)
  _ -> Left ["not Program"]
  where d = C.decode c


toProgram :: ProgramCode -> Program
toProgram (ProgramCode k m s) = Program (initializeVars k m) (makeCmds s)


initializeVars :: InputCode -> VarsCode -> Vars
initializeVars k m = M.fromList ([(v,0) | v <- [0..k+m]])


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

