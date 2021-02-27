module ExtendC.JumpBridge
  ( JumpBridge(..)
  , jumpBridge, toDefine, removeInit, makeVMap, changeVars
  , changeAllVars, toJump
  )
  where

import Control.Monad.State
import qualified Data.Map.Strict as M
import ExtendC.AST.Define
  ( Define(..), Stmt(..), Expr(..)
  , UniOpS(..), UniOpE(..), BinOp(..)
  )
import Data.Maybe (fromJust)
import ExtendC.Data.Functions (add)
import Util ( mapInd )



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
jumpBridge :: VMap -> JumpBridge
jumpBridge vmap = JumpBridge { variableIndex = M.size vmap }



-- Transforms


transform :: EStmt -> State JumpBridge [Stmt]
transform (Seq ss) = do
  aa <- mapM transform ss
  pure [Seq $ concat aa]
transform (Assign z (Bio Add (Var x) (Var y))) = do
  jump <- get
  let vs@[v1, v2, v3, v4] = makeVariables (variableIndex jump) 4
  updateVariables jump vs
  pure $ add x y z v1 v2 v3 v4
transform s = pure [s]


makeVariables :: Int -> Int -> [String]
makeVariables idx n  = map var [idx+1 .. idx+n]


updateVariables :: JumpBridge -> [String] -> State JumpBridge ()
updateVariables jump vs = do
  put $ jump { variableIndex = variableIndex jump + length vs }