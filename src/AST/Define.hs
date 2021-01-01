module AST.Define
  ( Define(..), Stmt(..), Expr(..)
  , UniOpS(..), UniOpE(..), BinOp(..)
  , fn
  )
  where



-- AST Define

data Define = Fn String [Expr] Stmt
            deriving (Show, Eq)

data Stmt = Nop
          | Assign String Expr
          | IfElse Expr Stmt Stmt
          | While Expr Stmt
          | Return Expr
          | Seq [Stmt]
          | Loop Expr Stmt
          | Init [Expr]
          | UnoS UniOpS Expr
          deriving (Show, Eq)

data Expr = Nat Integer
          | Var String
          | Con Bool
          | UnoE UniOpE Expr
          | Bio BinOp Expr Expr
          | Call String [Expr]
          deriving (Show, Eq)

data UniOpS = IncOp | DecOp
           deriving (Show, Eq)

data UniOpE =  Not | Neg
           deriving (Show, Eq)

data BinOp = Add | Sub | Mul | Div | Rem
           | Lt | Le | Gt | Ge
           | Eq | Neq
           | Or | And
           deriving (Show, Eq)





-- Stateで全変数を持ったほうがいいかも


fn :: Define -> Define
fn (Fn s e ss) = Fn s e $ Seq (stmt ss)

-- FIXME: clean
-- TODO: 変数宣言のマージ
stmt :: Stmt -> [Stmt]
stmt (Seq ss) = [Seq $ concatMap stmt ss]
stmt (Assign z (Bio Add (Var x) (Var y)))
  = add x y z "v0" "v1" "r"
stmt s = [s]


eliminateAtirhOp :: Define -> Define
eliminateAtirhOp = undefined


-- 引数分の変数の追加
addVariables :: Define -> Define
addVariables = undefined

-- result分の変数の追加
addReturn :: Define -> Define
addReturn = undefined


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

