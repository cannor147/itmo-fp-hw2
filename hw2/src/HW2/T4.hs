module HW2.T4
  ( State(..)
  , mapState
  , wrapState
  , extractAnnotated
  , joinState
  , modifyState
  , Prim(..)
  , Expr(..)
  , eval
  ) where

import           Control.Monad (ap)
import           HW2.T1

data State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f S{ runS = annotator } = S { runS = mapAnnotated f . annotator }

wrapState :: a -> State s a
wrapState value = S { runS = (:#) value }

extractAnnotated :: Annotated s (State s a) -> Annotated s a
extractAnnotated (state :# y) = runS state y

joinState :: State s (State s a) -> State s a
joinState S{ runS = annotator } = S { runS = extractAnnotated . annotator }

modifyState :: (s -> s) -> State s ()
modifyState modifier = S { runS = (:#) () . modifier }

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a
  = Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  x + y         = Op (Add x y)
  x * y         = Op (Mul x y)
  x - y         = Op (Sub x y)
  abs x         = Op (Abs x)
  signum x      = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)

eval :: Expr -> State [Prim Double] Double
eval (Val value) = pure value
eval (Op (Add x y)) = S { runS = (:#) (v1 + v2) . (++) ((Add v1 v2) : a2 ++ a1) }
  where
    (v1 :# a1) = runS (eval x) []
    (v2 :# a2) = runS (eval y) []
eval (Op (Mul x y)) = S { runS = (:#) (v1 * v2) . (++) ((Mul v1 v2) : a2 ++ a1) }
  where
    (v1 :# a1) = runS (eval x) []
    (v2 :# a2) = runS (eval y) []
eval (Op (Sub x y)) = S { runS = (:#) (v1 - v2) . (++) ((Sub v1 v2) : a2 ++ a1) }
  where
    (v1 :# a1) = runS (eval x) []
    (v2 :# a2) = runS (eval y) []
eval (Op (Div x y)) = S { runS = (:#) (v1 / v2) . (++) ((Div v1 v2) : a2 ++ a1) }
  where
    (v1 :# a1) = runS (eval x) []
    (v2 :# a2) = runS (eval y) []
eval (Op (Abs x)) = S { runS = (:#) (abs v1) . (++) ((Abs v1) : a1) }
  where
    (v1 :# a1) = runS (eval x) []
eval (Op (Sgn x)) = S { runS = (:#) (signum v1) . (++) ((Sgn v1) : a1) }
  where
    (v1 :# a1) = runS (eval x) []
