module DefineSpec (spec) where

import Test.Hspec
import Text.ParserCombinators.Parsec (parseTest, parse)
import qualified Data.Map.Strict as M
import AST.JumpBridge
    ( JumpBridge(..)
    , fromDefine, toDefine, pre, makeVMap, changeVars
    )
import AST.Define ( Define(..), Stmt(..), Expr(..), UniOpS(..), UniOpE(..), BinOp(..) )

spec :: Spec
spec = do
    describe "" $ do
        it "fromDefine" $ do
            let inputStmt = Seq [ Init [Var "z"]
                                , Assign "z" (Bio Add (Var "x") (Var "y"))
                                , Return (Var "z")
                                ]
            let inputArgs = [Var "x",Var "y"]
            fromDefine inputStmt inputArgs `shouldBe`
                JumpBridge { variables = ["v3"]
                           , variableIndex = 3
                           }

        it "vmap" $ do
            -- FIXME: 元のコードをcleanして、テストコードもclean
            let stmts = Seq [ Init [Var "z"]
                                , Assign "z" (Bio Add (Var "x") (Var "y"))
                                , Return (Var "z")
                                ]
            let args = [Var "x",Var "y"]
            makeVMap stmts args `shouldBe` M.fromList [("x", "v1"), ("y", "v2"), ("z", "v3")]

        it "changeVars" $ do
            -- FIXME: 元のコードをcleanして、テストコードもclean
            let vmap = M.fromList [("x", "v1"), ("y", "v2"), ("z", "v3")]
            let stmts = Seq [ Init [Var "z"]
                            , Assign "z" (Bio Add (Var "x") (Var "y"))
                            , Return (Var "z")
                            ]
            changeVars vmap stmts `shouldBe`
                Seq [ Init [Var "z"]
                    , Assign "v3" (Bio Add (Var "v1") (Var "v2"))
                    , Return (Var "v3")
                    ]

        it "pre" $ do
            -- FIXME: 元のコードをcleanして、テストコードもclean
            let inputStmt = Seq [ Init [Var "z"]
                                , Assign "v3" (Bio Add (Var "v1") (Var "v2"))
                                , Return (Var "v3")
                                ]
            let inputArgs = [Var "x",Var "y"]
            pre inputStmt inputArgs `shouldBe`
                Seq [ Assign "v1" (Var "x")
                    , Assign "v2" (Var "y")
                    , Assign "v3" (Bio Add (Var "v1") (Var "v2"))
                    , Return (Var "v3")
                    ]

        it "toDefine" $ do
            -- FIXME: テストコード自体が間違っている
            let pred = Seq [ Assign "v1" (Var "x")
                           , Assign "v2" (Var "y")
                           , Assign "v3" (Bio Add (Var "v1") (Var "v2"))
                           , Return (Var "v3")
                           ]
            let inputJump = JumpBridge { variables = ["v3"]
                                       , variableIndex = 3
                                       }
            toDefine pred inputJump `shouldBe`
                Seq [ Init [Var "v1", Var "v2", Var "v3",Var "v4",Var "v5",Var "v6",Var "v7",Var "v8",Var "v9"]
                    , Assign "v1" (Var "x")
                    , Assign "v2" (Var "y")
                    , Assign "v7" (Var "v4")
                    , Assign "v8" (Var "v5")
                    , Assign "v9" (Nat 0)
                    , Assign "a" (Var "v8")
                    , UnoS IncOp (Var "a")
                    , IfElse (Bio Gt (Var "a") (Nat 0)) (Seq [Loop (Var "v8") (UnoS IncOp (Var "v7"))
                    , Assign "v9" (Var "v7")]) (Seq [Assign "v8" (UnoE Neg (Var "v8"))
                    , Loop (Var "v8") (UnoS DecOp (Var "v7"))
                    , Assign "v9" (Var "v7")])
                    , Assign "v6" (Var "v9")
                    , Return (Var "v3")]