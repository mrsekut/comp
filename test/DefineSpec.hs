module DefineSpec (spec) where

import Test.Hspec
import Text.ParserCombinators.Parsec (parseTest, parse)
import qualified Data.Map.Strict as M
import ExtendC.JumpBridge
  ( JumpBridge(..)
  , jumpBridge, toDefine, removeInit, makeVMap, changeVars
  , changeAllVars, toJump
  )
import ExtendC.AST.Define ( Define(..), Stmt(..), Expr(..), UniOpS(..), UniOpE(..), BinOp(..) )

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

        it "toDefine" $ do
            let pred = Seq [ Assign "v3" (Bio Add (Var "v1") (Var "v2"))
                           , Return (Var "v3")
                           ]
            let inputJump = JumpBridge { variableIndex = 3 }
            toDefine pred inputJump `shouldBe`
                Seq [ Init [Var "v3",Var "v4",Var "v5",Var "v6",Var "v7"]
                    , Assign "v4" (Var "v1"),Assign "v5" (Var "v2"),Assign "v7" (Nat 0),Assign "v6" (Var "v5"),UnoS IncOp (Var "v6"),IfElse (Bio Gt (Var "v6") (Nat 0)) (Seq [Loop (Var "v5") (UnoS IncOp (Var "v4")),Assign "v7" (Var "v4")]) (Seq [Assign "v5" (UnoE Neg (Var "v5")),Loop (Var "v5") (UnoS DecOp (Var "v4")),Assign "v7" (Var "v4")]),Assign "v3" (Var "v7"),Return (Var "v3")]

        it "to jump code" $ do
            let input = Fn "hoge" [Var "x",Var "y"] (Seq [ Init [Var "z"], Assign "z" (Bio Add (Var "x") (Var "y")), Return (Var "z") ])
            toJump input `shouldBe`
                Fn "hoge" [Var "v1",Var "v2"]
                (Seq [ Init [Var "v3",Var "v4",Var "v5",Var "v6",Var "v7"]
                     , Assign "v4" (Var "v1")
                     , Assign "v5" (Var "v2")
                     , Assign "v7" (Nat 0)
                     , Assign "v6" (Var "v5")
                     , UnoS IncOp (Var "v6")
                     , IfElse (Bio Gt (Var "v6") (Nat 0))
                        (Seq [ Loop (Var "v5") (UnoS IncOp (Var "v4"))
                             , Assign "v7" (Var "v4")])
                        (Seq [ Assign "v5" (UnoE Neg (Var "v5"))
                             , Loop (Var "v5") (UnoS DecOp (Var "v4"))
                             , Assign "v7" (Var "v4")])
                     , Assign "v3" (Var "v7")
                     , Return (Var "v3")
                     ])
