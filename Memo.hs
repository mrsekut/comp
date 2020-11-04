-- module Memo where

-- import qualified Data.Map as Map
-- import qualified Code as P -- primitive


-- -- 標準入力など
-- main :: IO ()
-- main = undefined

-- type Code = Integer
-- comp :: Code -> Code -> Either Integer String
-- comp = undefined


-- type Vars = Map.Map Int Integer
-- data Program = Program Vars [Cmd]

-- type Line = Integer
-- data Cmd = Goto Line
--          | Bind Integer Integer
--          | BindV Integer Integer
--          | Inc Integer
--          | Dec Integer
--          | If Integer Integer






-- -- Parser的な
-- {-

-- Code 213797904982138037454632940231947778583451643946214351540021648522868699116020532367794717279183597514183155623196270804266008911831504391186653218399859236746627965664600576149708185774554408383297605366764401745745512661359368506
--  ↓
--  ↓ toProgramCode
--  ↓
-- ProgramCode 2 2 113410085239160792121509200101
-- ProgramCode 2 2 <3835,437,72,33,12>
-- ProgramCode 2 2 <[6,2,3],[1,6],[5,2],[4,1],[1,1]>
--  ↓
--  ↓ toProgram, makeCmds
--  ↓
-- Program 4 [(If 2 3),(Goto 6),(Dec 2),(Inc 1),(Goto 1)]

-- -}

-- -- FIXME: unsafe, Result
-- -- lengthが3であるのチェックも兼ねる
-- toProgramCode :: Code -> Result ProgramCode
-- toProgramCode c = case length d of
--     3 -> Right $ ProgramCode (d!!0) (d!!1) (d!!2)
--     _ -> Left ["not Program"]
--     where d = P.decode c


-- toProgram :: ProgramCode -> Program
-- toProgram (ProgramCode k m s) = Program (initializeVars k m) (makeCmds s)

-- initializeVars :: InputCode -> VarsCode -> Vars
-- initializeVars = undefined

-- makeCmds :: CmdsCode -> [Cmd]
-- makeCmds c = map toCmd $ P.decode c

-- -- isType
-- toCmd :: Code -> Cmd
-- toCmd c = case head d of
--     1 -> checkType1 d
--     2 -> checkType2 d
--     3 -> checkType3 d
--     4 -> checkType4 d
--     5 -> checkType5 d
--     6 -> checkType6 d
--     _ -> error "ooo"
--     where d = P.decode c


-- checkType1 :: [Integer] -> Cmd
-- checkType1 ix = if length ix == 2
--     then Goto (ix!!1)
--     else error "type error 1"

-- checkType2 :: [Integer] -> Cmd
-- checkType2 ix = if length ix == 3
--     then Bind (ix!!1) (ix!!2)
--     else error "type error 2"

-- checkType3 :: [Integer] -> Cmd
-- checkType3 ix = if length ix == 3
--     then BindV (ix!!1) (ix!!2)
--     else error "type error 3"

-- checkType4 :: [Integer] -> Cmd
-- checkType4 ix = if length ix == 2
--     then Inc (ix!!1)
--     else error "type error 4"

-- checkType5 :: [Integer] -> Cmd
-- checkType5 ix = if length ix == 2
--     then Dec (ix!!1)
--     else error "type error 5"

-- checkType6 :: [Integer] -> Cmd
-- checkType6 ix = if length ix == 3
--     then If (ix!!1) (ix!!2)
--     else error "type error 6"








-- -- executable
-- type Result a = Validation [String] a

-- executable :: Code -> Integer -> Result Int
-- executable p x = g <$> isProgramCode p <*> checkArgs p x

-- g :: () -> () -> Int
-- g _ _ = 1


-- checkArgs :: Code -> Code -> Result ()
-- checkArgs = undefined
-- -- checkArgs c i =  argLength c == length' i


-- -- isProgramCode



-- type InputCode = Integer
-- type VarsCode = Integer
-- type CmdsCode = Integer
-- data ProgramCode = ProgramCode InputCode VarsCode CmdsCode deriving (Show)



-- -- >>> isProgramCode 16  == Right ()
-- isProgramCode :: Code -> Result ()
-- -- isProgramCode c = f <$> checkLength c <*> checkVars c <*> checkType c <*> checkMax c
-- isProgramCode c = do
--     -- let (ProgramCode inps vars cmds) = toProgramCode c
--     undefined

-- f :: () -> () -> () -> b
-- f = error "not implemented"



-- type Validation = Either -- FIXME: 後で消す

-- -- k<mを満たしているか
-- -- >>> checkVars k m
-- checkVars :: InputCode -> VarsCode -> Bool
-- checkVars = (<)

-- -- Sに当たる箇所が、p.46のタイプ1~6のいずれかの形になっているか
-- checkType :: Code -> Validation [String] ()
-- checkType = undefined


-- -- ↑に出てくる変数の最大値が、m以下になっているか
-- checkMax :: Code -> Validation [String] ()
-- checkMax = undefined

-- -- タイプ1の`goto Ln`の`n`が実際のSの数より小さい
-- checkGoto :: Code -> Validation [String] ()
-- checkGoto = undefined

-- -- 	タイプ6の`if(va>0)goto Lb`の`b`も同様
-- checkIf :: Code -> Validation [String] ()
-- checkIf = undefined




-- argLength :: Code -> Integer
-- argLength c =  undefined
--     --    len i
--     -- where ProgramCode i _ _ = toProgramCode c



















