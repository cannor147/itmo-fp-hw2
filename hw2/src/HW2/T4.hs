module HW2.T4
  ( Expr(..)
  , Prim(..)
  , State(..)
  , BinaryOperation
  , UnaryOperation
  , calculate
  , correlatedBimap
  , eval
  , evalS
  , mapState
  , modifyState
  , joinState
  , wrapState
  ) where

import           Control.Arrow  ((>>>))
import           Control.Monad  (ap)
import           Data.Bifunctor (bimap, first)
import           HW2.T1
import           HW2.T2         (distAnnotated, wrapAnnotated)

-- | Custom implementation of state.
data State s a = S { runS :: s -> Annotated s a }

-- | Maps values in custom state.
mapState :: (a -> b) -> State s a -> State s b
mapState f S { runS = annotator } = S { runS = mapAnnotated f . annotator }

-- | Wraps custom state over value.
wrapState :: a -> State s a
wrapState value = S { runS = (:#) value }

-- | Extracts custom annotated value from nested custom state in annotated value.
extractAnnotated :: Annotated s (State s a) -> Annotated s a
extractAnnotated (state :# annotation) = runS state annotation

-- | Joins nested custom states.
joinState :: State s (State s a) -> State s a
joinState S { runS = annotator } = S { runS = extractAnnotated . annotator }

-- | Creates instance of custom state with modifying function for annotation.
modifyState :: (s -> s) -> State s ()
modifyState modifier = S { runS = (:#) () . modifier }

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

-- | Custom implementation of mathematical operations.
data Prim a
  = Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

-- | Calculates value from custom mathematical operation.
calculate :: Fractional a => Prim a -> a
calculate (Add x y) = x + y
calculate (Sub x y) = x - y
calculate (Mul x y) = x * y
calculate (Div x y) = x / y
calculate (Abs x)   = abs x
calculate (Sgn x)   = signum x

-- | Custom implementation of mathematical expression.
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

-- | Custom unary operation.
type UnaryOperation = Double -> Prim Double

-- | Custom binary operation.
type BinaryOperation = Double -> Double -> Prim Double

-- | Result for evaluation state.
type EvaluationResult = Annotated [Prim Double] (Prim Double)

-- | Maps values in pair with correlated second mapper with first mapper result.
correlatedBimap :: (a -> b) -> (b -> a -> b) -> ((a, a) -> (b, b))
correlatedBimap f g (x, y) = let xResult = f x in (xResult, (g xResult y))

-- | Gets evaluation result from custom unary operation and argument.
getUnaryEval :: UnaryOperation -> Expr -> EvaluationResult
getUnaryEval op = eval
  >>> flip runS mempty
  >>> mapAnnotated op

-- | Gets evaluation result from custom binary operation and arguments.
getBinaryEval :: BinaryOperation -> (Expr, Expr) -> EvaluationResult
getBinaryEval op = bimap eval eval
  >>> correlatedBimap (flip runS mempty) (\(_ :# array) -> flip runS array)
  >>> first (\(value :# _) -> wrapAnnotated value)
  >>> distAnnotated
  >>> mapAnnotated (uncurry op)

-- | Evaluates expression into state.
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

-- | Well-named version of eval.
evalS :: Expr -> State [Prim Double] Double
evalS = eval
