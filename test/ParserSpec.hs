module ParserSpec (spec) where

import Test.Hspec
import Text.ParserCombinators.Parsec (parseTest, parse)
import Parser

spec :: Spec
spec = do
    describe "program" $ do
        it "program" $ do
            let input = "fn hoge(a,b) {return 1+1;}"
            testParseProgram input `shouldBe` [Fn "hoge" [Var "a",Var "b"] (Seq [Return (Bio Add (Nat 1) (Nat 1))])]

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


testParseProgram :: String -> [Define]
testParseProgram input = case parseProgram input of
  Left{}    -> error "Parse failure"
  Right str -> str
