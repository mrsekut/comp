module DefineSpec (spec) where

import Test.Hspec
import Text.ParserCombinators.Parsec (parseTest, parse)
import AST.JumpBridge
    ( JumpBridge(..)
    , fromDefine, toDefine
    )
import AST.Define ( Define(..), Stmt(..), Expr(..), UniOpS(..), UniOpE(..), BinOp(..) )

spec :: Spec
spec = do
    describe "" $ do
        it "fromDefine" $ do
            let input = Seq [ Init [Var "z"], Assign "z" (Bio Add (Var "x") (Var "y")), Return (Var "z") ]
            fromDefine input `shouldBe` JumpBridge {variables = ["z"], variableIndex = 0}

        it "fromDefine" $ do
            let inputStmt = Seq [ Assign "z" (Bio Add (Var "x") (Var "y")), Return (Var "z") ]
            let inputJump = JumpBridge {variables = ["z"], variableIndex = 0}
            toDefine inputStmt inputJump `shouldBe`
                Seq [ Init [Var "z", Var "v1", Var "v2", Var "v3", Var "v4", Var "v5", Var "v6"]
                    , Assign "v4" (Var "v1")
                    , Assign "v5" (Var "v2")
                    , Assign "v6" (Nat 0)
                    , Assign "a" (Var "v5")
                    , UnoS IncOp (Var "a")
                    , IfElse (Bio Gt (Var "a") (Nat 0)) (Seq [Loop (Var "v5") (UnoS IncOp (Var "v4"))
                    , Assign "v6" (Var "v4")]) (Seq [Assign "v5" (UnoE Neg (Var "v5"))
                    , Loop (Var "v5") (UnoS DecOp (Var "v4"))
                    , Assign "v6" (Var "v4")])
                    , Assign "v3" (Var "v6")
                    , Return (Var "z")]

