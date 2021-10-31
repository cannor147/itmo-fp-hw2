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

import           Control.Monad  (ap)
import           Data.Bifunctor (bimap)
import           HW2.T1
import           HW2.T2

data State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f S { runS = annotator } = S { runS = mapAnnotated f . annotator }

wrapState :: a -> State s a
wrapState value = S { runS = (:#) value }

extractAnnotated :: Annotated s (State s a) -> Annotated s a
extractAnnotated (state :# y) = runS state y

joinState :: State s (State s a) -> State s a
joinState S { runS = annotator } = S { runS = extractAnnotated . annotator }

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

calculate :: Fractional a => Prim a -> a
calculate (Add x y) = x + y
calculate (Sub x y) = x - y
calculate (Mul x y) = x * y
calculate (Div x y) = x / y
calculate (Abs x)   = abs x
calculate (Sgn x)   = signum x

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

type UnaryOperation = Double -> Prim Double

type BinaryOperation = Double -> Double -> Prim Double

getUnaryEval :: UnaryOperation -> Expr -> Annotated [Prim Double] (Prim Double)
getUnaryEval op = mapAnnotated op . getEval

getBinaryEval :: BinaryOperation -> (Expr, Expr) -> Annotated [Prim Double] (Prim Double)
getBinaryEval op = (mapAnnotated (uncurry op) . distAnnotated) . bimap getEval getEval

getEval :: Expr -> Annotated [Prim Double] Double
getEval arg = runS (eval arg) mempty

eval :: Expr -> State [Prim Double] Double
eval (Val value) = pure value
eval (Op operation) = do
  let (prim :# annotation) = case operation of
        (Add x y) -> getBinaryEval Add (x, y)
        (Sub x y) -> getBinaryEval Sub (x, y)
        (Mul x y) -> getBinaryEval Mul (x, y)
        (Div x y) -> getBinaryEval Div (x, y)
        (Abs x)   -> getUnaryEval Abs x
        (Sgn x)   -> getUnaryEval Sgn x
  calculate prim <$ modifyState ((<>) (prim : annotation))
