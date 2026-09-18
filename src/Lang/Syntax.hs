-- | Абстрактный синтаксис мини-языка.
module Lang.Syntax where

import Text.PrettyPrint.GenericPretty
import MetaUtils

-- | Перечень бинарных операций.
data BinOp = Plus | Mult | Less | LessEq | Equal

-- | Синоним предметной области для типа имён переменных в AST.
type Name = String

-- | Имя конструктора.
type Ctor = String

-- | Деревья выражений.
data Expr
  = Const Int
  | Var Name
  | BinOp BinOp Expr Expr
  | Match Expr [Branch]

-- | Паттерны.
data Pattern
  = ConstPat Int
  | VarPat Name
  | CtorPat Ctor [Pattern]

-- | Ветка паттерн-матчинга.
data Branch = Branch Pattern Expr

-- ~ сюда не смотрим ~
deriving stock instance Eq BinOp
deriving stock instance Generic BinOp
instance Out BinOp
deriving via OutShow BinOp instance Show BinOp

deriving stock instance Eq Pattern
deriving stock instance Generic Pattern
instance Out Pattern
deriving via OutShow Pattern instance Show Pattern

deriving stock instance Eq Branch
deriving stock instance Generic Branch
instance Out Branch
deriving via OutShow Branch instance Show Branch

deriving stock instance Eq Expr
deriving stock instance Generic Expr
instance Out Expr
deriving via OutShow Expr instance Show Expr
