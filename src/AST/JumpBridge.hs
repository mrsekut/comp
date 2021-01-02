module AST.JumpBridge
  ( JumpBridge(..)
  , fromDefine, toDefine
  )
  where

import Control.Monad.State
import AST.Define
  ( Define(..), Stmt(..), Expr(..)
  , UniOpS(..), UniOpE(..), BinOp(..)
  )



-- To Jump Code


toJump :: Define -> Define
toJump (Fn fname args stmts) = Fn fname args $ toDefine (elminateInit stmts) (fromDefine stmts)


elminateInit :: Stmt -> Stmt
elminateInit (Seq ss) = Seq $ filter isNotInit ss
elminateInit ss = ss


isNotInit :: Stmt -> Bool
isNotInit (Init _) = False
isNotInit _        = True



-- AST JumpBridge

data JumpBridge =
 JumpBridge { variables :: [String]
            , variableIndex :: Int
            }
  deriving (Show, Eq)


toDefine :: Stmt -> JumpBridge -> Stmt
toDefine ss jmp = setInits init stms
  where
    (stms, jump) = runState (transform ss) jmp
    init = Init $ map Var $ variables jump


setInits :: Stmt -> [Stmt] -> Stmt
setInits inits stmts = Seq $ inits:seq
  where (Seq seq) = head stmts


-- FIXME: この段階で、`v2`のような変数を全て別物に変えておきたい
-- Transformしやすい形に変形する
fromDefine :: Stmt -> JumpBridge
fromDefine stmts =
   JumpBridge { variables = getInits stmts
              , variableIndex = 0
              }


getInits :: Stmt -> [String]
getInits (Seq ss) = concatMap getInits ss
getInits (Init es) = map (\(Var v) -> v) es
getInits _         = []



-- Transforms


transform :: Stmt -> State JumpBridge [Stmt]
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
makeVariables idx n  = map (\x -> "v" ++ show x) [idx+1 .. idx+n]


updateVariables :: JumpBridge -> [String] -> State JumpBridge ()
updateVariables jump vs = do
  put $ jump { variables = variables jump ++ vs
             , variableIndex = variableIndex jump + length vs
             }



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

