module HW2.T1
  ( Annotated(..)
  , Except(..)
  , Fun(..)
  , List(..)
  , Option(..)
  , Pair(..)
  , Prioritised(..)
  , Quad(..)
  , Stream(..)
  , Tree(..)
  , mapAnnotated
  , mapExcept
  , mapFun
  , mapList
  , mapOption
  , mapPair
  , mapPrioritised
  , mapQuad
  , mapStream
  , mapTree
  ) where

-- | Custom implementation of optional.
data Option a = None | Some a
  deriving Show

-- | Maps value in custom optional.
mapOption :: (a -> b) -> (Option a -> Option b)
mapOption _ None         = None
mapOption f (Some value) = Some (f value)

-- | Custom implementation of pair.
data Pair a = P a a
  deriving Show

-- | Maps values in custom pair.
mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P first second) = P (f first) (f second)

-- | Custom implementation of quad.
data Quad a = Q a a a a
  deriving Show

-- | Maps values in custom quad.
mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q first second third fourth) = Q (f first) (f second) (f third) (f fourth)

-- | Custom implementation of annotated value.
data Annotated e a = a :# e
  deriving Show
infix 0 :#

-- | Maps value in custom annotated value.
mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (value :# annotation) = f value :# annotation

-- | Custom implementation of either with exception semantics.
data Except e a = Error e | Success a
  deriving Show

-- | Maps value in custom error either.
mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error error')  = Error error'
mapExcept f (Success value) = Success (f value)

-- | Custom implementation of prioritised value.
data Prioritised a = Low a | Medium a | High a
  deriving Show

-- | Maps value in custom prioritised value.
mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low value)    = Low (f value)
mapPrioritised f (Medium value) = Medium (f value)
mapPrioritised f (High value)   = High (f value)

-- | Custom implementation of stream.
data Stream a = a :> Stream a
  deriving Show
infixr 5 :>

-- | Maps values in custom stream.
mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (head' :> tail') = f head' :> mapStream f tail'

-- | Custom implementation of list.
data List a = Nil | a :. List a
  deriving Show
infixr 5 :.

-- | Maps values in custom list.
mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil              = Nil
mapList f (head' :. tail') = f head' :. mapList f tail'

-- | Custom implementation of function object.
data Fun i a = F (i -> a)

-- | Custom implementation of (.).
(<-=-) :: (b -> c) -> (a -> b) -> (a -> c)
(<-=-) f g x = f (g x)

-- | Concatenates values in custom function objects.
mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F function) = F (f <-=- function)

-- | Custom implementation of list.
data Tree a = Leaf | Branch (Tree a) a (Tree a)
  deriving Show

-- | Maps values in custom tree.
mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf                      = Leaf
mapTree f (Branch left value right) = Branch (mapTree f left) (f value) (mapTree f right)
