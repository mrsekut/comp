module Data.Functions (add) where

import AST.Define ( Define(..), Stmt(..), Expr(..)
                  , UniOpS(..), UniOpE(..), BinOp(..)
                  )



-- External Types


type EInput = String
type EOutput = String



-- Internal Types


type IInput = String
type IOutput = String
type IVar = String



-- Four arithmetic operations


add :: EInput -> EInput -> EOutput
    -> IInput -> IInput -> IOutput -> IVar
    -> [Stmt]
add x y z v0 v1 a r
  = [ Assign v0 (Var x)
    , Assign v1 (Var y)
    , Assign r  (Nat 0)
    , Assign a (Var v1)
    , UnoS IncOp (Var a)
    , IfElse (Bio Gt (Var a) (Nat 0))
      (Seq [ Loop (Var v1) (UnoS IncOp (Var v0))
           , Assign r (Var v0)])
      (Seq [ Assign v1 (UnoE Neg (Var v1))
           , Loop (Var v1) (UnoS DecOp (Var v0))
           , Assign r (Var v0)])
    , Assign z (Var r)
    ]