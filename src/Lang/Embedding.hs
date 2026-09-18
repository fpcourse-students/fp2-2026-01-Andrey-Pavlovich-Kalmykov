-- | Средствами Haskell можно сделать задание синтаксических
-- деревьев мини-языка более простым и привычным.
module Lang.Embedding where

import Lang.Syntax
import MetaUtils

c :: Int -> Expr
c = Const

v :: Name -> Expr
v = Var

new :: Ctor -> [Expr] -> Expr
new = todo "new"

infixl 6 .+
infixl 6 .-
infixl 7 .*
infixl 7 ./
infix 4 .<
infix 4 .<=
infix 4 .==
(.+), (.-), (.*), (./), (.<), (.<=), (.==) :: Expr -> Expr -> Expr
(.+) = BinOp Plus
(.-) = todo ".-"
(.*) = BinOp Mult
(./) = todo "./"
(.<) = BinOp Less
(.<=) = BinOp LessEq
(.==) = BinOp Equal

pc :: Int -> Pattern
pc = ConstPat

pv :: Name -> Pattern
pv = VarPat

ctor :: Ctor -> [Pattern] -> Pattern
ctor = CtorPat

branch :: Pattern -> Expr -> Branch
branch = Branch

match :: Expr -> [Branch] -> Expr
match = Match
