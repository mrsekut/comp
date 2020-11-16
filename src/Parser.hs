module Parser where

import Text.Parsec.Expr
    ( buildExpressionParser, Assoc(AssocLeft), Operator(..) )
import Text.ParserCombinators.Parsec
    (alphaNum, char, lower, (<?>), (<|>), try, Parser ,many, parse, ParseError)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
    ( GenLanguageDef(..), emptyDef )


data Define = Fn String [Expr] Stmt
            | Call
            deriving Show

-- TODO: loop
data Stmt = Nop
          | String := Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | Return Expr
          | Seq [Stmt]
          deriving Show
-- TODO: 型宣言 `int a,b,c,d,..;`
-- TODO: call
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
  identLetter = alphaNum <|> char '_',
  reservedOpNames =
    [ "++", "--"
    , "+", "-", "*", "/", "%"
    , "==", "!=", "<=", "<", ">=", ">"
    , "&&", "||", "!"
    , "=", ","
    ],
  reservedNames =
    [ "true", "false"
    , "if", "then", "else", "while", "loop", "return", "fn"
    ],
  caseSensitive = True
}

lexer = P.makeTokenParser def
parens = P.parens lexer
identifier = P.identifier lexer
reservedOp = P.reservedOp lexer
reserved = P.reserved lexer
natural = P.natural lexer
semi = P.semi lexer
braces = P.braces lexer
commaSep = P.commaSep lexer
whiteSpace = P.whiteSpace lexer



-- Program

parseProgram :: String -> Either ParseError [Define]
parseProgram = parse (whiteSpace >> many parseFn) "myparser"



-- Function


parseFn :: Parser Define
parseFn = do
  reserved "fn"
  name <- identifier
  args <- parens $ commaSep parseExpr
  Fn name args <$> parseSeq


-- Statement


parseSeq :: Parser Stmt
parseSeq = braces $ do
  list <- many parseStmt
  return $ if null list then Nop else Seq list


parseStmt :: Parser Stmt
parseStmt = parseIf
        <|> parseSeq
        <|> parseAssign
        <|> parseWhile
        <|> parseReturn
        <|> parseNop


parseIf :: Parser Stmt
parseIf = do
  reserved "if"
  cond <- parens parseExpr
  s1 <- parseStmt
  s2 <- try (reserved "else" >> parseStmt) <|> pure Nop
  pure $ If cond s1 s2


parseWhile :: Parser Stmt
parseWhile = do
  reserved "while"
  cond <- parens parseExpr
  While cond <$> parseStmt


parseReturn :: Parser Stmt
parseReturn = do
  reserved "return"
  expr <- parseExpr
  semi
  return $ Return expr


parseAssign :: Parser Stmt
parseAssign = do
  name <- identifier
  reservedOp "="
  expr <- parseExpr
  semi
  return $ name := expr


parseNop :: Parser Stmt
parseNop = semi >> pure Nop



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

-- TODO: call
parseTerm =
  parens parseExpr
    <|> Nat <$> natural
    <|> fmap Var identifier
    <|> (reserved "true" >> pure (Con True))
    <|> (reserved "false" >> pure (Con False))