module Parser (parseProgram, parseStmt, parseExpr, Define(..), Stmt(..), Expr(..), UniOp(..), BinOp(..)) where

import Text.Parsec.Expr
    ( buildExpressionParser, Assoc(AssocLeft), Operator(..) )
import Text.ParserCombinators.Parsec
    (alphaNum, char, lower, (<?>), (<|>), try, Parser, many, parse, ParseError, sepBy)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
    ( GenLanguageDef(..), emptyDef )


data Define = Fn String [Expr] Stmt
            deriving (Show ,Eq)

data Stmt = Nop
          | Assign String Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | Return Expr
          | Seq [Stmt]
          | Loop Expr Stmt
          deriving (Show, Eq)

data Expr = Nat Integer
          | Var String
          | Con Bool
          | Uno UniOp Expr
          | Bio BinOp Expr Expr
          | Call String [Expr]
          deriving (Show, Eq)

data UniOp = Inc | Dec
           | Not
           | Neg
           deriving (Show ,Eq)

data BinOp = Add | Sub | Mul | Div | Rem
           | Lt | Le | Gt | Ge
           | Eq | Neq
           | Or | And
           deriving (Show, Eq)




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
  pure $ if null list then Nop else Seq list


parseStmt :: Parser Stmt
parseStmt = parseSeq
        <|> parseIf
        <|> parseLoop
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


parseLoop :: Parser Stmt
parseLoop = do
  reserved "loop"
  cnt <- parens parseExpr
  Loop cnt <$> parseStmt


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
  pure $ Return expr


parseAssign :: Parser Stmt
parseAssign = do
  name <- identifier
  reservedOp "="
  expr <- parseExpr
  semi
  pure $ Assign name expr


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

parseTerm =
  parens parseExpr
    <|> Nat <$> natural
    <|> parseCall
    <|> fmap Var identifier
    <|> (reserved "true" >> pure (Con True))
    <|> (reserved "false" >> pure (Con False))


parseCall = do
  id <- identifier
  try
    (do args <- parens $ sepBy parseExpr $ reservedOp ","
        pure $ Call id args)
    <|> pure (Var id)