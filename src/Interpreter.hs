module Interpreter where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Control.Monad.Except

data Expr = Var String
          | ExprNum Integer
          | ExprString String
          | ExprBool Bool
          | App Expr [Expr]
          | Assign Expr Expr
          | Cond Expr Expr Expr
          deriving Show


parseExpr :: Parser Expr
parseExpr = try parseCond
         <|> try parseApp
         <|> try parseBinOp
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
  op <- try comparisons <|> ((\x->[x]) `fmap` oneOf operators)
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


data Val = Number Integer
         | String String
         | Bool Bool
         | List [Val]
         | Prim ([Val] -> ThrowsError Val)
         | Function [String] [Expr]

showVal :: Val -> String
showVal (Prim f) = "<function>"
showVal (Number n) = show n
showVal (Bool n) = show n
showVal (List ls) = concat $ map showVal ls
showVal (String n) = show n

instance Show Val where
  show = showVal

data EvalError = UnboundVar String
               | TypeMismatch String
               | NotFunction String
               deriving Show

type ThrowsError = Either EvalError
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


eval :: Expr -> ThrowsError Val
eval (ExprString str) = return $ String str
eval (ExprBool b) = return $ Bool b
eval (ExprNum n) = return $ Number n
eval (Cond pred consq alt) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    otherwise  -> eval consq
eval (Var x) = maybe (throwError $ UnboundVar x)
                     return
                     (lookup x globalEnv)
eval (App f args) = do
  func  <- eval f
  args' <- mapM eval args
  apply func args'

apply :: Val -> [Val] -> ThrowsError Val
apply (Prim f) args = f args
apply g _ = throwError $ NotFunction (show g)

globalEnv :: [(String, Val)]
globalEnv = [("+", Prim sumVals),
             ("-", Prim subVals),
             ("*", Prim multVals),
             ("&", Prim allVals),
             ("|", Prim anyVals),
             ("==", Prim eqVals),
             ("<", Prim ltVals),
             (">", Prim gtVals),
             ("<=", Prim lteqVals),
             (">=", Prim gteqVals),
             ("var", Number 1),
             ("var1", String "variable 1"),
             ("var2", Number 10),
             ("sum", Prim sumVals)]


sumVals, multVals, subVals, anyVals, allVals, gtVals, ltVals, lteqVals, gteqVals :: [Val] -> ThrowsError Val
sumVals  = foldrM binAdd
multVals = foldrM binMult
subVals  = foldrM binSub
allVals  = foldrM (binBool (&&))
anyVals  = foldrM (binBool (||))
eqVals = foldrM (binEq (==))
gtVals = foldrM (binEq (>))
ltVals = foldrM (binEq (<))
lteqVals = foldrM (binEq (<=))
gteqVals = foldrM (binEq (>=))
-- eqVals = undefined
-- gtVals = undefined
-- ltVals = undefined
-- lteqVals = undefined
-- gteqVals = undefined


binAdd, binSub, binMult :: Val -> Val -> ThrowsError Val
binAdd (Number x) (Number y) = return $ Number (x+y)
binAdd (String s1) (String s2) = return $ String (s1 ++ s2)
binAdd _ _ = throwError $ TypeMismatch "cannot add"

binSub (Number x) (Number y) = return $ Number (x - y)
binSub _ _ = throwError $ TypeMismatch "cannot subtract non-integers"

binMult (Number x) (Number y) = return $ Number (x*y)
binMult _ _ = throwError $ TypeMismatch "cannot multiply non-integers"

binBool :: (Bool -> Bool -> Bool) -> Val -> Val -> ThrowsError Val
binBool op (Bool b1) (Bool b2) = return $ Bool (op b1 b2)
binBool _ _ _ = throwError $ TypeMismatch "not booleans"

binEq :: (Integer -> Integer -> Bool) -> Val -> Val -> ThrowsError Val
binEq op (Number x) (Number y) = return $ Bool (op x y)
binEq _ _ _ = throwError $ TypeMismatch "cannot compare"


foldrM :: (Val -> Val -> ThrowsError Val) -> [Val] -> ThrowsError Val
foldrM _ [v]    = return v
foldrM f (v:vs) = do
  v' <- foldrM f vs
  f v v'

-- arithPrim :: (Integer -> Integer -> Integer) -> [Val] -> ThrowsError Val
-- arithPrim op args = mapM unpackNum args >>= return . Number . foldr1 op

-- unpackNum :: Val -> ThrowsError Integer
-- unpackNum (Number n) = return n
-- unpackNum _ = throwError $ TypeMismatch "not a number"
