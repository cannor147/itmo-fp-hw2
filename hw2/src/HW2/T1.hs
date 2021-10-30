module HW2.T1
  ( Option(..)
  , Pair(..)
  , Quad(..)
  , Annotated(..)
  , Except(..)
  , Prioritised(..)
  , Stream(..)
  , List(..)
  , Fun(..)
  , Tree(..)
  , mapOption
  , mapPair
  , mapQuad
  , mapAnnotated
  , mapExcept
  , mapPrioritised
  , mapStream
  , mapList
  , mapFun
  , mapTree
  ) where


data Option a = None | Some a

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption _ None         = None
mapOption f (Some value) = Some $ f value

data Pair a = P a a

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P first second) = P (f first) (f second)

data Quad a = Q a a a a

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q first second third fourth) = Q (f first) (f second) (f third) (f fourth)

data Annotated e a = a :# e
infix 0 :#

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (value :# annotation) = f value :# annotation

data Except e a = Error e | Success a

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error error')  = Error error'
mapExcept f (Success value) = Success $ f value

data Prioritised a = Low a | Medium a | High a

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low value)    = Low $ f value
mapPrioritised f (Medium value) = Medium $ f value
mapPrioritised f (High value)   = High $ f value

data Stream a = a :> Stream a
infixr 5 :>

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (head' :> tail') = f head' :> mapStream f tail'

data List a = Nil | a :. List a
infixr 5 :.

mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil              = Nil
mapList f (head' :. tail') = f head' :. mapList f tail'

data Fun i a = F (i -> a)

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F function) = F (f . function)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf                      = Leaf
mapTree f (Branch left value right) = Branch (mapTree f left) (f value) (mapTree f right)
