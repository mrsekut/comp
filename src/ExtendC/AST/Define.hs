module ExtendC.AST.Define
  ( Define(..), Stmt(..), Expr(..)
  , UniOpS(..), UniOpE(..), BinOp(..)
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