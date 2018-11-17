module Interpreter where

import Control.Monad.Except
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.Semigroup
import Parser


data Val = Number Integer
         | String String
         | Bool Bool
         | List [Val]
         | Prim ([Val] -> Eval Val)
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
               | Empty
               deriving Show

instance Semigroup EvalError where
  _ <> e = e


instance Monoid EvalError where
  mempty = Empty
  mappend = (<>)


type ThrowsError = Either EvalError
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


eval :: Expr -> Eval Val
eval (ExprString str) = return $ String str
eval (ExprBool b) = return $ Bool b
eval (ExprNum n) = return $ Number n
eval (Assign name expr) = do
  value <- eval expr
  env   <- get
  let env' = setValue env name value
  put env'
  return value

eval (Cond pred consq alt) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    otherwise  -> eval consq

eval (Var x) = do
  env <- get
  maybe (throwError $ UnboundVar x) return (lookupVar env x)

eval (App f args) = do
  func  <- eval f
  args' <- mapM eval args
  apply func args'

apply :: Val -> [Val] -> Eval Val
apply (Prim f) args = f args
apply g _ = throwError $ NotFunction (show g)

-- environment is a list of frames (extended env)
type Env = [Frame]
-- each frame is a list of bindings
type Frame = [(String, Val)]

lookupVar :: Env -> String -> Maybe Val
lookupVar env name = msum $ map (lookup name) env

setValue :: Env -> String -> Val -> Env
setValue (f:fs) name value = ((name, value):f) : fs


type Eval a = ExceptT EvalError (StateT Env IO) a

runEval :: Env -> Eval a -> IO (Either EvalError a, Env)
runEval env ev = runStateT (runExceptT ev) env

runEval' :: Env -> Eval a -> IO ()
runEval' env ev = runEval env ev >> (return ())

evaluate :: Env -> Expr -> IO (ThrowsError Val)
evaluate env expr = fst `fmap` runEval env (eval expr)

globalEnv :: Env
globalEnv = [[("+", Prim sumVals),
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
             ("sum", Prim sumVals)]]


sumVals, multVals, subVals, anyVals, allVals, gtVals, ltVals, lteqVals, gteqVals :: [Val] -> Eval Val
sumVals  = foldrM binAdd
multVals = foldrM binMult
subVals  = foldrM binSub
allVals  = compBool (&&)
anyVals  = compBool (||)
eqVals vs   = compNum (==) vs <|> compStr (==) vs <|> compBool (==) vs
gtVals vs   = compNum (>) vs <|> compStr (>) vs <|> compBool (>) vs
ltVals vs   = compNum (<) vs <|> compStr (<) vs <|> compBool (<) vs
lteqVals vs = compNum (<=) vs <|> compStr (<=) vs <|> compBool (<=) vs
gteqVals vs = compNum (>=) vs <|> compStr (>=) vs <|> compBool (>=) vs

compStr op = foldrM (binBool unpackStr op)
compNum op = foldrM (binBool unpackNum op)
compBool op = foldrM (binBool unpackBool op)

binAdd, binSub, binMult :: Val -> Val -> Eval Val
binAdd (Number x) (Number y) = return $ Number (x+y)
binAdd (String s1) (String s2) = return $ String (s1 ++ s2)
binAdd _ _ = throwError $ TypeMismatch "cannot add"

binSub (Number x) (Number y) = return $ Number (x - y)
binSub _ _ = throwError $ TypeMismatch "cannot subtract non-integers"

binMult (Number x) (Number y) = return $ Number (x*y)
binMult _ _ = throwError $ TypeMismatch "cannot multiply non-integers"

binBool :: (Val -> String -> Eval a) -> (a -> a -> Bool) -> Val -> Val -> Eval Val
binBool unpacker op val1 val2 = do
  left  <- unpacker val1 "Cannot compare"
  right <- unpacker val2 "Cannot compare"
  return $ Bool (left `op` right)

unpackBool :: Val -> String -> Eval Bool
unpackBool (Bool b) _ = return b
unpackBool _ msg = throwError $ TypeMismatch msg

unpackNum :: Val -> String -> Eval Integer
unpackNum (Number x) _ = return x
unpackNum _ msg = throwError $ TypeMismatch msg

unpackStr :: Val -> String -> Eval String
unpackStr (String s) _ = return s
unpackStr _ msg = throwError $ TypeMismatch msg



foldrM :: (a -> a -> Eval a) -> [a] -> Eval a
foldrM _ [v]    = return v
foldrM f (v:vs) = do
  v' <- foldrM f vs
  f v v'
