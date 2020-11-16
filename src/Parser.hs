module Parser where

import Text.Parsec.Expr
    ( buildExpressionParser,
      Assoc(AssocLeft),
      Operator(Postfix, Infix, Prefix) )
import Text.ParserCombinators.Parsec
    ( alphaNum, char, lower, (<?>), (<|>), try, Parser )
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
    ( GenLanguageDef(commentStart, commentEnd, commentLine, identStart,
                     identLetter, reservedOpNames, reservedNames, caseSensitive),
      emptyDef )


-- TODO: func, call
data Stmt = Nop
          | String := Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | Return Expr
          | Seq [Stmt]
          deriving Show

-- TODO: 型宣言 `int a,b,c,d,..;`
data Expr = Nat Integer
          | Var String
          | Con Bool
          | Uno UniOp Expr
          | Bio BinOp Expr Expr
          deriving Show
data UniOp = Inc | Dec
           | Not
           | Neg
           deriving Show
data BinOp = Add | Sub | Mul | Div | Rem
           | Lt | Le | Gt | Ge
           | Eq | Neq
           | Or | And
           deriving Show




def = emptyDef {
  commentStart = "/*", commentEnd = "*/", commentLine = "//",
  identStart = lower,
  identLetter = alphaNum <|> (char '_'),
  reservedOpNames = [
    "++", "--",
    "+", "-", "*", "/", "%",
      "==", "!=", "<=", "<", ">=", ">",
    "&&", "||", "!",
    "=", ","],
  reservedNames = [
    "true", "false",
    "if", "then", "else", "while", "loop", "return"],
  caseSensitive = True
}

lexer = P.makeTokenParser def
parens = P.parens lexer
identifier = P.identifier lexer
reservedOp = P.reservedOp lexer
reserved = P.reserved lexer
natural = P.natural lexer
semi = P.semi lexer



-- Expression

parseExpr :: Parser Expr
parseExpr = buildExpressionParser exprOps parseTerm <?> "expression"


exprOps =
  [ [postfix "++" (Uno Inc), postfix "--" (Uno Dec)]
  , [prefix "!" (Uno Not), prefix "-" (Uno Neg)]
  , [binaryl "*" (Bio Mul), binaryl "/" (Bio Div), binaryl "%" (Bio Rem)]
  , [binaryl "+" (Bio Add), binaryl "-" (Bio Sub)]
  , [binaryl "<" (Bio Lt), binaryl "<=" (Bio Le), binaryl ">" (Bio Gt), binaryl ">=" (Bio Ge), binaryl "<=" (Bio Sub)]
  , [binaryl "==" (Bio Eq), binaryl "!=" (Bio Neq)]
  , [binaryl "||" (Bio Or), binaryl "&&" (Bio And)]
  ]


binaryl name fun = Infix (reservedOp name >> pure fun) AssocLeft
prefix name fun  = Prefix (reservedOp name >> pure fun)
postfix name fun = Postfix (reservedOp name >> pure fun)



-- Term

parseTerm =
  parens parseExpr
    <|> Nat <$> natural
    <|> fmap Var identifier
    <|> (reserved "true" >> pure (Con True))
    <|> (reserved "false" >> pure (Con False))
