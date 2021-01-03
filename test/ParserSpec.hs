module ParserSpec (spec) where

import Test.Hspec
import Text.ParserCombinators.Parsec (parseTest, parse)
import ExtendC.Parser
import ExtendC.AST.Define ( Define(..), Stmt(..), Expr(..), UniOpS(..), BinOp(..) )

spec :: Spec
spec = do
    describe "program" $ do
        it "program" $ do
            let input = "fn hoge(a,b) {r = a + b; return(1);}"
            testParseProgram input `shouldBe` [Fn "hoge" [Var "a",Var "b"] (Seq [Assign "r" (Bio Add (Var "a") (Var "b")),Return (Nat 1)])]

    describe "statements" $ do
        it "if" $ do
            let input = "if (true) { a = 1; b = 2; }"
            testParseStmt input `shouldBe` IfElse (Con True) (Seq [Assign "a" (Nat 1), Assign "b" (Nat 2)]) Nop
        it "loop" $ do
            let input = "loop(2) { a = a + 1; }"
            testParseStmt input `shouldBe` Loop (Nat 2) (Seq [Assign "a" (Bio Add (Var "a") (Nat 1))])
        it "initialize" $ do
            let input = "int a, b, c;"
            testParseStmt input `shouldBe` Init [Var "a",Var "b",Var "c"]
        it "inc" $ do
            let input = "i++;"
            testParseStmt input `shouldBe` UnoS IncOp (Var "i")


    describe "expr" $ do
        it "terms" $ do
            let input = "element(p, 1)"
            testParseExpr input `shouldBe` Call "element" [Var "p",Nat 1]

    describe "temrs" $ do
        it "terms" $ do
            let input = "1+2"
            testParseExpr input `shouldBe` Bio Add (Nat 1) (Nat 2)



testParseExpr :: String -> Expr
testParseExpr input = case parse parseExpr "Example" input of
  Left{}    -> error "Parse failure"
  Right str -> str


testParseStmt :: String -> Stmt
testParseStmt input = case parse parseStmt "Example" input of
  Left{}    -> error "Parse failure"
  Right str -> str



testParseProgram :: String -> [Define]
testParseProgram input = case parseProgram input of
  Left{}    -> error "Parse failure"
  Right str -> str
