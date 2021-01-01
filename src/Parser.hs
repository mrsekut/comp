module Parser
  ( main
  , parseProgram , parseStmt , parseExpr
  , parseProgramDebug
  )
  where

import AST.Define
  ( Define(..), Stmt(..), Expr(..)
  , UniOpS(..), UniOpE(..), BinOp(..)
  )
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.Parsec.Expr
    ( Assoc(AssocLeft), Operator(..)
    , buildExpressionParser
    )
import Text.ParserCombinators.Parsec
    ( Parser, ParseError
    , alphaNum, char, lower, try, many, parse,  sepBy
    , (<?>), (<|>)
    )
import Text.ParserCombinators.Parsec.Language
    ( GenLanguageDef(..), emptyDef )



-- Main

main :: IO ()
main = do
  cs <- readC "./samples/C/add.c"
  print cs


readC :: FilePath -> IO [Define]
readC filePath = do
  src <- readFile filePath
  case parseProgram src of
      Right xs -> pure xs
      _ -> error "parse error"



-- Program

parseProgram :: String -> Either ParseError [Define]
parseProgram = parse (whiteSpace >> many parseFn) "myparser"


parseProgramDebug :: String -> Define
parseProgramDebug src = case parse (whiteSpace >> many parseFn) "myparser" src of
  Right xs -> head xs
  _ -> error "parse error"



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
        <|> parseInc
        <|> parseDec
        <|> parseIf
        <|> parseLoop
        <|> parseAssign
        <|> parseWhile
        <|> parseReturn
        <|> parseInit
        <|> parseNop


parseIf :: Parser Stmt
parseIf = do
  reserved "if"
  cond <- parens parseExpr
  s1 <- parseStmt
  s2 <- try (reserved "else" >> parseStmt) <|> pure Nop
  pure $ IfElse cond s1 s2


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


parseInit :: Parser Stmt
parseInit = do
  reserved "int"
  vars <- commaSep parseExpr
  semi
  pure $ Init vars


parseInc :: Parser Stmt
parseInc = try $ do
  n <- parseExpr
  reservedOp "++"
  semi
  pure $ UnoS IncOp n


parseDec :: Parser Stmt
parseDec = try $ do
  n <- parseExpr
  reservedOp "--"
  semi
  pure $ UnoS DecOp n


parseNop :: Parser Stmt
parseNop = semi >> pure Nop



-- Expression

parseExpr :: Parser Expr
parseExpr = buildExpressionParser exprOps parseTerm <?> "expression"


exprOps =
  [ [prefix "!" (UnoE Not), prefix "-" (UnoE Neg)]
  , [binaryl "*" (Bio Mul), binaryl "/" (Bio Div), binaryl "%" (Bio Rem)]
  , [binaryl "+" (Bio Add), binaryl "-" (Bio Sub)]
  , [binaryl "<" (Bio Lt), binaryl "<=" (Bio Le), binaryl ">" (Bio Gt), binaryl ">=" (Bio Ge), binaryl "<=" (Bio Sub)]
  , [binaryl "==" (Bio Eq), binaryl "!=" (Bio Neq)]
  , [binaryl "||" (Bio Or), binaryl "&&" (Bio And)]
  ]


binaryl name fun = Infix (reservedOp name >> pure fun) AssocLeft
prefix name fun  = Prefix (reservedOp name >> pure fun)



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



-- Parsec Utils

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
    , "int"
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