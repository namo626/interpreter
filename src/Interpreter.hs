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
         | Function (Maybe String) [String] [Expr] Env  -- Function name [parameter] <body> <env>

showVal :: Val -> String
showVal (Prim f) = "<primitive>"
showVal (Number n) = show n
showVal (Bool n) = show n
showVal (List ls) = concat $ map showVal ls
showVal (String n) = show n
showVal (Function _ _ _ _) = "<function>"

instance Show Val where
  show = showVal

data EvalError = UnboundVar String
               | TypeMismatch String
               | NotFunction String
               | Exit
               | Empty
               deriving Show

instance Semigroup EvalError where
  -- associative law
  Empty <> x = x
  x <> Empty = x
  x <> y = y


instance Monoid EvalError where
  -- mempty must be the identity element, that is
  -- Empty `mappend` x = x
  -- x `mappend` Empty = x
  -- mappend must be associative
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

eval (Lambda params body) = do
  env <- get
  return $ Function Nothing params body env

eval (Def name params body) = do
  env      <- get
  let func = Function (Just name) params body env
      env' = defineVar env name func
  put env'
  return $ String $ "Function \"" ++ name ++ "\" defined"


eval (Assign name expr) = do
  value <- eval expr
  env   <- get
  let env' = defineVar env name value
  put env'
  return $ String "Value assigned"

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


-- APPLY -------------------------------------------------------------
apply :: Val -> [Val] -> Eval Val

apply (Prim f) args = f args

apply (Function Nothing params body env) args = do
  oldEnv <- get
  put $ extendEnv env params args
  val <- evalSequence body
  put oldEnv
  return val

apply proc@(Function (Just name) params body env) args = do
  oldEnv <- get
  put $ extendEnv env (name:params) (proc:args)
  val <- evalSequence body
  put oldEnv
  return val

apply g _ = throwError $ NotFunction (show g)

evalSequence :: [Expr] -> Eval Val
evalSequence [expr] = eval expr
evalSequence (e:es) = eval e >> evalSequence es


-- environment is a list of frames (extended env)
type Env = [Frame]
-- each frame is a list of bindings
type Frame = [(String, Val)]

lookupVar :: Env -> String -> Maybe Val
lookupVar env name = msum $ map (lookup name) env

defineVar :: Env -> String -> Val -> Env
defineVar [] _ _ = error "Environment is empty"
defineVar (f:fs) name value = case lookup name f of
  Nothing   -> ((name, value):f) : fs
  otherwise -> (loop f) : fs

  where
    searched = loop f
    loop [] = []
    loop (var:vars)
      | fst var == name = (name, value) : vars
      | otherwise       = var : (loop vars)


-- Extend an environment with a new frame
extendEnv :: Env -> [String] -> [Val] -> Env
extendEnv env vars values
  | length vars == length values = (zip vars values) : env
  | otherwise = error "Unequal amount of bindings"


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
  left  <- unpacker val1 "Cannot compare values of different types"
  right <- unpacker val2 "Cannot compare values of different types"
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
