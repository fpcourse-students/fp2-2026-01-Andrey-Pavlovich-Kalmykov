module Lang.Semantics where

import Control.Applicative (asum)
import Control.Monad (zipWithM)
import Data.Foldable (fold)
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Lang.Syntax
import MetaUtils

-- | Значения, в которые вычисляются выражения.
data Value = Number Int | Object Ctor [Value]
  deriving Eq

instance Show Value where
  show = \case
    Number value -> show value
    Object ctor values
      | null values -> ctor
      | otherwise -> "(" <> ctor <> " " <> unwords (show <$> values) <> ")"

-- | Булевы константы: объекты без полей.
trueValue, falseValue :: Value
trueValue = Object "True" []
falseValue = Object "False" []

-- | Состояние программы - это просто словарь из имён переменных в их значения.
-- Ознакомьтесь с интерфейсом Data.Map с помощью Hoogle.
type Env = Map Name Value

eval :: Expr -> Value
eval expr = evalExpr expr Map.empty

-- | Интерпретатор выражений.
evalExpr :: Expr -> Env -> Value
evalExpr expr env = case expr of
  Const value -> Number value
  Var name -> case env !? name of
    Nothing -> error $ "ERROR: Undefined variable '" <> name <> "'"
    Just value -> value
  BinOp op lhs rhs -> evalBinOp op (evalExpr lhs env) (evalExpr rhs env)
  Match expr' brs ->
    let val = evalExpr expr' env
        matchPattern pat value env' = case (pat, value) of
          (ConstPat n, Number n') -> if n == n' then Just env' else Nothing
          (VarPat name, _) -> Just (Map.insert name value env')
          (CtorPat tag ps, Object tag' vs) ->
            if tag == tag' && length ps == length vs
            then matchPats ps vs env'
            else Nothing
          _ -> Nothing
        matchPats [] [] env' = Just env'
        matchPats (p:ps) (v:vs) env' = case matchPattern p v env' of
          Just env'' -> matchPats ps vs env''
          Nothing -> Nothing
        matchPats _ _ _ = Nothing
        tryBranch (Branch pat expr'') = case matchPattern pat val env of
          Just newEnv -> Just (evalExpr expr'' newEnv)
          Nothing -> Nothing
    in case asum (map tryBranch brs) of
         Just result -> result
         Nothing -> error "ERROR: No matching branch"
  Ctor tag xs -> Object tag $ map (flip evalExpr env) xs

-- | Интерпретатор бинарных операций.
evalBinOp :: BinOp -> Value -> Value -> Value
evalBinOp op = case op of
  Plus -> wrapNum (+)
  Minus -> wrapNum (-)
  Mult -> wrapNum (*)
  Div -> wrapNum div
  Less -> wrapBool (<)
  LessEq -> wrapBool (<=)
  Equal -> wrapBool (==)
  where
    wrapNum op lhs rhs = Number $ unpackNum lhs `op` unpackNum rhs
    wrapBool op lhs rhs = if unpackNum lhs `op` unpackNum rhs then trueValue else falseValue
    unpackNum = \case
      Number value -> value
      obj@Object {} -> error $ "TYPE_ERROR: Expected number, got object " <> show obj
