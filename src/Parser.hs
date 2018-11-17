module Parser
  (
    Expr(..),
    readExpr
  )
  where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
data Expr = Var String
          | ExprNum Integer
          | ExprString String
          | ExprBool Bool
          | App Expr [Expr]
          | Assign String Expr
          | Cond Expr Expr Expr
          deriving Show


parseExpr :: Parser Expr
parseExpr = try parseCond
         <|> try parseApp
         <|> try parseBinOp
         <|> try parseAssign
         <|> parseVar
         <|> parseNum
         <|> parseString
         <|> parseParens

readExpr :: String -> Expr
readExpr input =
  case parse (do {result <- parseExpr; noMore; return result}) "python" input of
    Left err  -> ExprString $ "No match" ++ show err
    Right val -> val

noMore :: Parser ()
noMore = spaces >> eof

parseAssign :: Parser Expr
parseAssign = do
  (Var str) <- parseVar
  spaces
  char '='
  spaces
  value <- parseExpr
  return $ Assign str value

parseCond :: Parser Expr
parseCond = do
  string "if"
  skipMany1 space
  pred <- parseExpr
  skipMany1 space
  consq <- parseExpr
  skipMany1 space
  string "else"
  skipMany1 space
  alt <- parseExpr
  return $ Cond pred consq alt
  -- newline
  -- consq <- parseExpr
  -- newline
  -- string "else:"
  -- newline
  -- alt <- parseExpr
  -- return $ Cond pred consq alt

operators :: String
operators = "+-*/|&<>"

comparisons :: Parser String
comparisons = string "=="
           <|> string ">="
           <|> string "<="

parseParens :: Parser Expr
parseParens = do
  char '('
  expr <- parseExpr
  char ')'
  return expr

parseBinOp :: Parser Expr
parseBinOp = do
  expr1 <- parseParens <|> parseNum <|> parseVar <|> parseString
  spaces
  op <- try comparisons <|> ((\x->[x]) `fmap` oneOf operators)
  spaces
  expr2 <- parseParens <|> parseNum <|> parseVar <|> parseString
  return $ App (Var op) [expr1, expr2]

parseVar :: Parser Expr
parseVar = do
  c <- letter
  cs <- many (letter <|> digit)
  let x = c:cs
  return $ case x of
    "True"  -> ExprBool True
    "False" -> ExprBool False
    _       -> Var x

parseNum :: Parser Expr
parseNum = do
  x <- many1 digit
  return $ ExprNum $ read x

parseApp :: Parser Expr
parseApp = do
  c <- letter
  cs <- many (letter <|> digit)
  char '('
  args <- sepBy parseExpr commaSep'
  char ')'
  return $ App (Var (c:cs)) args

commaSep' :: Parser Char
commaSep' = do
  spaces
  c <- char ','
  spaces
  return c

parseString :: Parser Expr
parseString = do
  char '"'
  xs <- many $ noneOf "\""
  char '"'
  return $ ExprString xs
