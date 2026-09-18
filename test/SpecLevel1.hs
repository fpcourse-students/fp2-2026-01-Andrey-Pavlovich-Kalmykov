module SpecLevel1 where

import Control.DeepSeq
import Control.Exception
import Data.Map qualified as Map
import Data.Function (fix)
import Lang.Embedding
import Lang.Programs
import Lang.Semantics
import Lang.Syntax
import Test.Prelude

tests :: NamedTests
tests = nameTests 1
  [ testMinusDiv
  , testObjs
  , testPatternMatching
  ]

deriving stock instance Generic Value
deriving via GenericArbitrary Value instance Arbitrary Value
instance NFData Value

data Tree a = Leaf | Node a [Tree a]
  deriving (Eq, Show)

deriving stock instance Generic (Tree a)
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized $ fix \rec -> \case
    0 -> pure Leaf
    n -> oneof
      [ pure Leaf
      , choose (0, 3) >>= \len -> Node <$> arbitrary <*> vectorOf len (rec $ n - 1)
      ]
  shrink = genericShrink

testMinusDiv :: Test
testMinusDiv = TestList
  [ propertyToTest "minus works" \(x, y) ->
      Number (x - y) === eval (c x .- c y)
  , propertyToTest "div works" \(x, NonZero y) ->
      Number (x `div` y) === eval (c x ./ c y)
  , TestCase $ assertEqual "displacement works" (Number 400) $
      evalExpr displacement $ Map.fromList [("v", Number 50), ("v0", Number 30), ("a", Number 2)]
  ]

testObjs :: Test
testObjs = TestList
  [ propertyToTest "ctors work for list" \(xs :: [Int]) ->
      listToValue (Number <$> xs) === eval (listToCtors $ c <$> xs)
  , propertyToTest "ctors work for tree" \(t :: Tree Int) ->
      treeToValue t === eval (treeToCtors t)
  , TestCase $ assertEqual "[1,2,3]" (listToCtors [c 1, c 2, c 3]) list123
  ]
  where
    listToCtors :: [Expr] -> Expr
    listToCtors = foldr (\x tail -> new "Cons" [x, tail]) (new "Nil" [])

    listToValue :: [Value] -> Value
    listToValue = foldr (\x tail -> Object "Cons" [x, tail]) (Object "Nil" [])

    treeToCtors :: Tree Int -> Expr
    treeToCtors = \case
      Leaf -> new "Leaf" []
      Node x ts -> new "Node" [c x, listToCtors (treeToCtors <$> ts)]

    treeToValue :: Tree Int -> Value
    treeToValue = \case
      Leaf -> Object "Leaf" []
      Node x ts -> Object "Node" [Number x, listToValue (map treeToValue ts)]

testPatternMatching :: Test
testPatternMatching = TestList
  [ TestCase $ assertEqual "const pattern" (Number 1) $ eval $
      match (c 42) [branch (pc 42) $ c 1]
  , TestCase $ assertEqual "match second" (Number 6) $ eval $
      match (c 42)
      [ branch (pc 1) $ c 5
      , branch (pc 42) $ c 6
      ]
  , TestCase $ assertThrows @ErrorCall $ eval $
      match (c 42)
      [ branch (pc 1) $ c 43
      , branch (pc 2) $ c 43
      ]
  , TestCase $ assertEqual "var pattern" (Number 43) $ eval $
      match (c 42) [branch (pv "x") $ v "x" .+ c 1]
  , TestCase $ assertEqual "var pattern overrides scope" (Number 42) $ eval $
      match (c 1)
      [ branch (pv "x") $ match (c 42)
        [ branch (pv "x") $ v "x"
        ]
      ]
  , TestCase $ assertEqual "first match" (Number 42) $ eval $
      match (c 1)
      [ branch (pv "x") $ v "x" .+ c 41
      , branch (pv "y") $ v "x" .+ v "y" .+ c 100
      ]
  , TestCase $ assertThrows @ErrorCall $ eval $
      match (c 1)
      [ branch (pv "x") $ v "x" .+ v "y"
      , branch (pv "y") $ c 0
      ]
  , TestCase $ assertEqual "match ctor pattern no args" (Number 1) $ eval $
      match (new "Nil" []) [branch (ctor "Nil" []) $ c 1]
  , TestCase $ assertEqual "var pattern" (Object "Pair" [Number 1, Object "Nil" []]) $ eval $
      match (new "Cons" [c 1, new "Nil" []])
      [ branch (ctor "Nil" []) $ c 42
      , branch (ctor "Cons" [pv "x", pv "tail"]) $ new "Pair" [v "x", v "tail"]
      ]
  , TestCase $ assertEqual "nested patterns" (Object "Triple" [Number 1, Number 2, Object "Nil" []]) $ eval $
      match (new "Cons" [c 1, new "Cons" [c 2, new "Nil" []]])
      [ branch (ctor "Nil" []) $ c 42
      , branch (ctor "Cons" [pv "x", ctor "Nil" []]) $ c 43
      , branch (ctor "Cons" [pv "x", ctor "Cons" [pv "y", pv "tail"]]) $
          new "Triple" [v "x", v "y", v "tail"]
      ]
  , TestCase $ assertThrows @ErrorCall $ eval $
      match (new "C" [c 1, c 2]) [branch (ctor "C" [pv "x"]) $ c 4]
  ]
