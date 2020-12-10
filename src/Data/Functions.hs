module Data.Functions where

import AST.Define ( Define(..), Stmt(..), Expr(..)
                  , UniOpS(..), UniOpE(..), BinOp(..)
                  )



-- Four arithmetic operations

add :: [Define]
add =  [Fn "add" [Var "x",Var "y"] (Seq [Init [Var "a"],Assign "a" (Var "y"),UnoS Inc (Var "a"),If (Bio Gt (Var "a") (Nat 0)) (Seq [Loop (Var "y") (UnoS Inc (Var "x")),Return (Var "x")]) (Seq [Assign "y" (UnoE Neg (Var "y")),Loop (Var "y") (UnoS Dec (Var "x")),Return (Var "x")])])]
