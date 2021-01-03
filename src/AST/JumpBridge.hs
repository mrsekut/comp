module AST.JumpBridge
  ( JumpBridge(..)
  , fromDefine, toDefine, pre, makeVMap, changeVars

  -- for debug
  , var, mapInd
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
type VMap = M.Map String String



-- To Jump Code


toJump :: Define -> Define
toJump (Fn fname args stmts) = Fn fname args transfomedStmts
  where
    -- FIXME: clean: 処理が分散していてわかりにくい
    transfomedStmts = toDefine pred (fromDefine stmts args)
    vmap = makeVMap stmts args
    estmt = changeVars vmap stmts
    pred = pre estmt args


-- FIXME: name
-- FIXME: 内部で使われている引数と、変数を全て書き換える必要がある
-- Initの除去
-- 引数をAssignとして変数宣言
-- 元の引数の総書き換え
pre :: Stmt -> Args -> EStmt
pre stmt args = case stmt of
  Seq ss -> Seq $ argsAssign args ++ filter isNotInit ss
  _      -> stmt


isNotInit :: Stmt -> Bool
isNotInit (Init _) = False
isNotInit _        = True


argsAssign :: Args -> [Stmt]
argsAssign = mapInd (\a i -> Assign (var i) a)


makeVMap :: Stmt -> Args -> VMap
makeVMap stmt args = M.fromList $ zip vars $ mapInd (\_ i -> var i) vars
  where
    vars = map unVar args ++ getVars stmt



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




-- AST JumpBridge


data JumpBridge =
 JumpBridge { variables :: [String]
            , variableIndex :: Int
            }
  deriving (Show, Eq)


-- FIXME: name微妙かもな
-- 前処理をしたStmt,JumpBridgeを使用し、変換を実施する
toDefine :: EStmt -> JumpBridge -> Stmt
toDefine ss jmp = setInits init stms
  where
    (stms, jump) = runState (transform ss) jmp
    init = Init $ map (Var . var) [1..variableIndex jump]


setInits :: Stmt -> [Stmt] -> Stmt
setInits inits stmts = Seq $ inits:seq
  where (Seq seq) = head stmts


-- Transformしやすい形に変形する
fromDefine :: Stmt -> Args -> JumpBridge
fromDefine stmts args =
  JumpBridge { variables = makeVariables argsLen varsLen
             , variableIndex = argsLen + varsLen
             }
  where
    argsLen = length args
    vars = getVars stmts
    varsLen = length vars


getVars :: Stmt -> [String]
getVars (Seq ss)  = concatMap getVars ss
getVars (Init es) = map (\(Var v) -> v) es
getVars _         = []



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
  put $ jump { variables = variables jump ++ vs
             , variableIndex = variableIndex jump + length vs
             }



-- AST JumpBridge


var :: Int -> String
var n = "v" ++ show n

unVar :: Expr -> String
unVar (Var v) = v


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


-- TODO: Cの関数の引数と変数の兼ね合い
        -- まずテストコードなおす
-- TODO: Cの関数の最初の内部変数のそう書き換え
-- TODO: 今の出力は、引数のx, yと、もとの変数zがおかしい。ソレ以外は会っている
-- TODO: 元々のコードに`v1`のような変数があるとおかしくなる