-- | Примеры программ на мини-языке.
module Lang.Programs where

import Lang.Syntax
import Lang.Embedding

-- | Вычисляет пройденный путь с равномерным ускорением, если
-- v - конечная скорость, v0 - начальная скорость, a - ускорение.
displacement :: Expr
displacement = (v "v" .* v "v" .- v "v0" .* v "v0") ./ (c 2 .* v "a")

-- | Список из элементов 1, 2, 3.
list123 :: Expr
list123 = new "Cons" [c 1, new "Cons" [c 2, new "Cons" [c 3, new "Nil" []]]]

-- | Вычисляет минимум из чисел a и b.
minAB :: Expr
minAB = match (v "a" .<= v "b")
  [ branch (ctor "True" []) $ v "a"
  , branch (ctor "False" []) $ v "b"
  ]
