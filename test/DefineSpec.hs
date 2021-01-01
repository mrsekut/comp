module DefineSpec (spec) where

import Test.Hspec
import Text.ParserCombinators.Parsec (parseTest, parse)
import Parser
import AST.Define ( Define(..), Stmt(..), Expr(..), UniOpS(..), BinOp(..) )

-- FIXME:
spec :: Spec
spec = do
    describe "program" $ do
        it "program" $ do
            let input = "fn hoge(a,b) {r = a + b; return(1);}"
            testParseProgram input `shouldBe` [Fn "hoge" [Var "a",Var "b"] (Seq [Assign "r" (Bio Add (Var "a") (Var "b")),Return (Nat 1)])]

testParseProgram :: String -> [Define]
testParseProgram input = case parseProgram input of
  Left{}    -> error "Parse failure"
  Right str -> str
