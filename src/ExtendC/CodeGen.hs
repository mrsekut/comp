{-# LANGUAGE OverloadedStrings #-}
module ExtendC.CodeGen where

import Control.Monad.RWS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import ExtendC.AST.Define ( Define(..), Stmt(..), Expr(..)
                          , UniOpS(..), UniOpE(..), BinOp(..)
                          )



-- W


type W a = RWST () Text State IO a
newtype State = State { variableCount :: Int }



-- Reifiable


class Reifiable a where
    reify :: a -> W Text


instance Reifiable Define where
    -- reify (Fn f as b) = do
    --     args <- mapM reify as
    --     let params = T.intercalate ", " args
    --     body <- reify b
    --     brs <- braces body
    --     pure $ T.concat ["fn ", T.pack f, "(", params, ")", brs]

    -- FIXME: debugがしづらいので, Cの書き方で出す
    reify (Fn f as b) = do
        args <- mapM reify as
        let params = T.intercalate ", int " args
        body <- reify b
        brs <- braces body
        pure $ T.concat ["int ", T.pack f, "(int ", params, ")", brs]

instance Reifiable Stmt where
    reify (Assign v e) = do
        exp <- reify e
        pure $ T.concat [T.pack v," = ",exp]
    reify (IfElse c t e) = do
        cnd <- reify c
        thn <- reify t
        els <- reify e
        pure $ T.concat ["if (", cnd, ") {\n\t", thn , "} else {\n\t", els, "}"]
    reify (While c b) = do
        cnd <- reify c
        body <- reify b
        brs <- braces body
        pure $ T.concat ["while (", cnd, ")", brs]
    reify (Return r) = do
        rtn <- reify r
        pure $ T.concat ["return (", rtn, ")"]
    reify (Seq ss) = do
        stmts <- mapM reify ss
        pure $ T.concat[T.intercalate ";\n" stmts, ";\n"]
    reify (Loop c b) = do
        cnt <- reify c
        body <- reify b
        brs <- braces body
        -- FIXME: debugがしづらいのでforで出す
        -- pure $ T.concat ["loop (", cnt, ")", brs]
        pure $ T.concat ["for (i=0; i<", cnt, "; i++)", brs]
    reify (Init vs) = do
        stmts <- mapM reify vs
        pure $ T.concat ["int ", T.intercalate "," stmts]
    reify (UnoS op e) = do
        exp <- reify e
        pure $ T.concat [exp, unoS op, ";"]
    reify Nop = pure $ T.pack ""


instance Reifiable Expr where
    reify (Nat i) = pure $ T.pack $ show i
    reify (Var v) = pure $ T.pack v
    reify (Con b) = pure $ T.pack $ show b
    reify (UnoE op e) = do
        exp <- reify e
        pure $ T.concat [unoE op, exp]
    reify (Bio op e1 e2) = do
        exp1 <- reify e1
        exp2 <- reify e2
        pure $ T.concat [exp1, bio op, exp2]
    reify (Call f ps) = do
        params <- mapM reify ps
        pure $ T.concat [T.pack f, "(", T.intercalate ";\n" params, ")", ";"]



-- Utils


unoS :: UniOpS -> Text
unoS IncOp = "++"
unoS DecOp = "--"


unoE :: UniOpE -> Text
unoE Not = "!"
unoE Neg = "-"


bio :: BinOp -> Text
bio Add = "+"
bio Sub = "-"
bio Mul = "*"
bio Div = "/"
bio Rem = "%"
bio Lt  = "<"
bio Le  = "<="
bio Gt  = ">"
bio Ge  = ">="
bio Eq  = "="
bio Neq = "!="
bio And = "&&"
bio Or  = "||"


braces :: Text -> W Text
braces body = do
    pure $ T.concat [" {\n", body, "}\n"]


gen :: Define -> IO ()
gen e = do
    (txt,_,assigned) <- runRWST (reify e) () (State 0)
    T.putStrLn $ T.append assigned txt
