module DefineSpec (spec) where

import Test.Hspec
import Text.ParserCombinators.Parsec (parseTest, parse)
import qualified Data.Map.Strict as M
import AST.JumpBridge
    ( JumpBridge(..) , jumpBridge
    , toDefine, removeInit, makeVMap, changeVars
    , changeAllVars, toJump
    )
import AST.Define ( Define(..), Stmt(..), Expr(..), UniOpS(..), UniOpE(..), BinOp(..) )

spec :: Spec
spec = do
    describe "" $ do
        it "change all variables" $ do
            let vmap = M.fromList [("x", "v1"), ("y", "v2"), ("z", "v3")]
            let args = [Var "x",Var "y"]
            let define = Seq [ Init [Var "z"]
                             , Assign "z" (Bio Add (Var "x") (Var "y"))
                             , Return (Var "z")
                             ]
            changeAllVars vmap args define `shouldBe`
                ( [Var "v1",Var "v2"]
                , Seq [ Init [Var "z"]
                      , Assign "v3" (Bio Add (Var "v1") (Var "v2"))
                      , Return (Var "v3")
                      ]
                )

        it "Define to JumpBridge" $ do
            let vmap = M.fromList [("x", "v1"), ("y", "v2"), ("z", "v3")]
            jumpBridge vmap `shouldBe`
                JumpBridge { variableIndex = 3 }

        it "vmap" $ do
            let args = [Var "x",Var "y"]
            let stmts = Seq [ Init [Var "z"]
                            , Assign "z" (Bio Add (Var "x") (Var "y"))
                            , Return (Var "z")
                            ]
            makeVMap args stmts  `shouldBe` M.fromList [("x", "v1"), ("y", "v2"), ("z", "v3")]

        it "changeVars" $ do
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

        it "removeInit" $ do
            let stmt = Seq [ Init [Var "z"]
                           , Assign "v3" (Bio Add (Var "v1") (Var "v2"))
                           , Return (Var "v3")
                           ]
            removeInit stmt `shouldBe`
                Seq [ Assign "v3" (Bio Add (Var "v1") (Var "v2"))
                    , Return (Var "v3")
                    ]

        -- it "toDefine" $ do
        --     -- FIXME: テストコードが間違っている
        --     let pred = Seq [ Assign "v3" (Bio Add (Var "v1") (Var "v2"))
        --                    , Return (Var "v3")
        --                    ]
        --     let inputJump = JumpBridge { variableIndex = 3 }
        --     toDefine pred inputJump `shouldBe`
        --         Seq [ Init [Var "v3",Var "v4",Var "v5",Var "v6",Var "v7",Var "v8",Var "v9"]
        --             , Assign "v7" (Var "v4")
        --             , Assign "v8" (Var "v5")
        --             , Assign "v9" (Nat 0)
        --             , Assign "a" (Var "v8")
        --             , UnoS IncOp (Var "a")
        --             , IfElse (Bio Gt (Var "a") (Nat 0)) (Seq [Loop (Var "v8") (UnoS IncOp (Var "v7"))
        --             , Assign "v9" (Var "v7")]) (Seq [Assign "v8" (UnoE Neg (Var "v8"))
        --             , Loop (Var "v8") (UnoS DecOp (Var "v7"))
        --             , Assign "v9" (Var "v7")])
        --             , Assign "v6" (Var "v9")
        --             , Return (Var "v3")]

        it "to jump code" $ do
            -- FIXME: テストコードが間違っている
            let input = Fn "hoge" [Var "x",Var "y"] (Seq [ Init [Var "z"], Assign "z" (Bio Add (Var "x") (Var "y")), Return (Var "z") ])
            toJump input `shouldBe`
                Fn "hoge" [Var "v1",Var "v2"]
                ( Seq [ Init [Var "v3",Var "v4",Var "v5",Var "v6",Var "v7",Var "v8",Var "v9"]
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
                      , Return (Var "v3")])
