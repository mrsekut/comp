module AST.JumpBridge
  ( JumpBridge(..)
  , jumpBridge, toDefine, removeInit, makeVMap, changeVars
  , changeAllVars, toJump
  )
  where

import Control.Monad.State
import qualified Data.Map.Strict as M
import AST.Define
  ( Define(..), Stmt(..), Expr(..)
  , UniOpS(..), UniOpE(..), BinOp(..)
  )
import Data.Maybe (fromJust)



-- Types


type Args = [Expr]
type EStmt = Stmt -- 前処理されたStmt



-- To Jump Code


toJump :: Define -> Define
toJump (Fn fname args stmt) = Fn fname eargs transfomedStmts
  where
    -- FIXME: clean: 処理が分散していてわかりにくい
    vmap = makeVMap args stmt
    (eargs, estmt) = changeAllVars vmap args stmt
    pred = removeInit estmt
    transfomedStmts = toDefine pred (jumpBridge vmap)


removeInit :: Stmt -> EStmt
removeInit stmt = case stmt of
  Seq ss -> Seq $ filter isNotInit ss
  _      -> stmt


isNotInit :: Stmt -> Bool
isNotInit (Init _) = False
isNotInit _        = True



-- Variables


type VMap = M.Map String String -- 初期変数のMap. 引数は含めない


-- ①引数、内部変数の総書き換え
changeAllVars :: VMap -> Args -> Stmt -> ([Expr], Stmt)
changeAllVars vmap args stmt = (eargs, estmt)
  where
    estmt = changeVars vmap stmt
    eargs = map (Var . var) [1..length args]


makeVMap :: Args -> Stmt -> VMap
makeVMap args stmt = M.fromList $ zip vars $ mapInd (\_ i -> var i) vars
  where
    vars = map unVar args ++ getVars stmt


getVars :: Stmt -> [String]
getVars (Seq ss)  = concatMap getVars ss
getVars (Init es) = map unVar es
getVars _         = []


-- FIXME: clean, name, classを使って統一するとか？
changeVars :: VMap -> Stmt -> EStmt
changeVars vmap stmt = case stmt of
  Seq stmt        -> Seq $ map (changeVars vmap) stmt
  Assign a expr   -> Assign (key a) (changeExpr expr)
  IfElse expr t e -> IfElse (changeExpr expr) t e
  Return expr     -> Return (changeExpr expr)
  Loop expr l     -> Loop (changeExpr expr) l
  UnoS op expr    -> UnoS op (changeExpr expr)
  _               -> stmt
  where
    key v = fromJust $ M.lookup v vmap
    changeExpr = changeVarsExpr vmap


-- FIXME: clean, name
changeVarsExpr :: VMap -> Expr -> Expr
changeVarsExpr vmap expr = case expr of
  Var s              -> key s
  UnoE op expr       -> UnoE op $ changeVarsExpr vmap expr
  Bio op expr1 expr2 -> Bio op (changeVarsExpr vmap expr1) (changeVarsExpr vmap expr2)
  Call fname exprs   -> Call fname $ map (changeVarsExpr vmap) exprs
  _                  -> expr
  where
    key v = Var $ fromJust $ M.lookup v vmap


var :: Int -> String
var n = "v" ++ show n

unVar :: Expr -> String
unVar (Var v) = v



-- AST JumpBridge


newtype JumpBridge = JumpBridge { variableIndex :: Int } deriving (Show, Eq)


-- FIXME: name微妙かもな, 引数名も微妙
-- 前処理をしたStmt,JumpBridgeを使用し、変換を実施する
toDefine :: EStmt -> JumpBridge -> Stmt
toDefine ss jmp = setInits init stms
  where
    (stms, jump) = runState (transform ss) jmp
    init = Init $ map (Var . var) [variableIndex jmp..variableIndex jump]


setInits :: Stmt -> [Stmt] -> Stmt
setInits inits stmts = Seq $ inits:seq
  where (Seq seq) = head stmts


-- Transformしやすい形に変形する
-- FIXME: name
jumpBridge :: VMap -> JumpBridge
jumpBridge vmap = JumpBridge { variableIndex = M.size vmap }



-- Transforms


transform :: EStmt -> State JumpBridge [Stmt]
transform (Seq ss) = do
  aa <- mapM transform ss
  pure [Seq $ concat aa]
transform (Assign z (Bio Add (Var x) (Var y))) = do
  jump <- get
  let vs@[v1, v2, v3, v4, v5, v6] = makeVariables (variableIndex jump) 6
  updateVariables jump vs
  pure $ add v1 v2 v3 v4 v5 v6
transform s = pure [s]


makeVariables :: Int -> Int -> [String]
makeVariables idx n  = map var [idx+1 .. idx+n]


updateVariables :: JumpBridge -> [String] -> State JumpBridge ()
updateVariables jump vs = do
  put $ jump { variableIndex = variableIndex jump + length vs }



-- AST JumpBridge


-- FIXME: move to Utils
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [1..]



-- 中身を移植(変数の入れ替えなど)
add :: String -> String -> String -> String -> String -> String -> [Stmt]
add x y z v0 v1 r
  = [ Assign v0 (Var x)
    , Assign v1 (Var y)
    , Assign r  (Nat 0)

    , Assign "a" (Var v1)
    , UnoS IncOp (Var "a")
    , IfElse (Bio Gt (Var "a") (Nat 0))
      (Seq [ Loop (Var v1) (UnoS IncOp (Var v0))
           , Assign r (Var v0)])
      (Seq [ Assign v1 (UnoE Neg (Var v1))
           , Loop (Var v1) (UnoS DecOp (Var v0))
           , Assign r (Var v0)])

    , Assign z (Var r)
    ]


-- TODO: 引数と返り値(x,y,z)の出力が微妙におかしい, add関数の内部がおかしいのか？
-- TODO: debug手順ちゃんとまとめかないと時間アクト忘れる
-- TODO: add関数内の変数aの扱い
-- TODO: JumpBridge全体的にりふぁくた
-- TODO: add以外の関数をやっていく
-- TODO: genやCコードも含めた一気通貫のテストコードが必要(後にgotoなどの変換をするので)
-- ①引数、内部変数の総書き換え
-- ②JumpBridgeに変換
-- ③変換
-- ④もとに戻す