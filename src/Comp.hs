module Comp where

import qualified Data.Map as Map
import qualified Code as P -- primitive


-- 標準入力など
main :: IO ()
main = undefined

type Code = Integer
comp :: Code -> Code -> Either Integer String
comp = undefined


type Vars = Map.Map Int Integer
data Program = Program Vars [Cmd]

type Line = Integer
data Cmd = Goto Line
         | Bind Integer Integer
         | BindV Integer Integer
         | Inc Integer
         | Dec Integer
         | If Integer Integer






initialie :: Code -> Vars
initialie = undefined






-- executable
type Result a = Validation [String] a

executable :: Code -> Integer -> Result Int
executable p x = g <$> isProgramCode p <*> checkArgs p x

g :: () -> () -> Int
g _ _ = 1


checkArgs :: Code -> Code -> Result ()
checkArgs = undefined
-- checkArgs c i =  argLength c == length' i


-- isProgramCode



type InputCode = Integer
type VarsCode = Integer
type CmdsCode = Integer
data ProgramCode = ProgramCode InputCode VarsCode CmdsCode deriving (Show)


isProgramCode :: Code -> Result ()
-- isProgramCode c = f <$> checkLength c <*> checkVars c <*> checkType c <*> checkMax c
isProgramCode c = do
    -- let (ProgramCode inps vars cmds) = toProgramCode c
    undefined

f :: () -> () -> () -> b
f = error "not implemented"



-- FIXME: unsafe, Result
-- lengthが3であるのチェックも兼ねる
toProgramCode :: Code -> Result ProgramCode
toProgramCode c = case length d of
    3 -> Right $ ProgramCode (d!!0) (d!!1) (d!!2)
    _ -> Left ["not Program"]
    where d = P.decode c




type Validation = Either -- FIXME: 後で消す

-- k<mを満たしているか
checkVars :: InputCode -> VarsCode -> Bool
checkVars = (<)

-- Sに当たる箇所が、p.46のタイプ1~6のいずれかの形になっているか
checkType :: Code -> Validation [String] ()
checkType = undefined

-- ↑に出てくる変数の最大値が、m以下になっているか
checkMax :: Code -> Validation [String] ()
checkMax = undefined

-- タイプ1の`goto Ln`の`n`が実際のSの数より小さい
checkGoto :: Code -> Validation [String] ()
checkGoto = undefined

-- 	タイプ6の`if(va>0)goto Lb`の`b`も同様
checkIf :: Code -> Validation [String] ()
checkIf = undefined




argLength :: Code -> Integer
argLength c =  undefined
    --    len i
    -- where ProgramCode i _ _ = toProgramCode c

