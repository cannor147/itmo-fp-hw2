module HW2.T4
  ( Expr(..)
  , Prim(..)
  , State(..)
  , calculate
  , eval
  , evalM
  , evalS
  , mapState
  , modifyState
  , joinState
  , wrapState
  ) where

import           Control.Monad (ap)
import           HW2.T1

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
calculate :: Fractional f => Prim f -> f
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

-- | Evaluates expression into some Monad.
evalM :: (Monad m, Fractional f) => Expr -> (Prim f -> m f) -> m f
evalM (Val value)    _      = pure $ realToFrac value
evalM (Op operation) action = evalArgs operation
  where
    evalArgs (Add firstArg secondArg) = evalBinary Add firstArg secondArg
    evalArgs (Sub firstArg secondArg) = evalBinary Sub firstArg secondArg
    evalArgs (Mul firstArg secondArg) = evalBinary Mul firstArg secondArg
    evalArgs (Div firstArg secondArg) = evalBinary Div firstArg secondArg
    evalArgs (Abs firstArg)           = evalUnary Abs firstArg
    evalArgs (Sgn firstArg)           = evalUnary Sgn firstArg
    evalUnary operator firstArg = do
      firstResult <- evalM firstArg action
      action $ operator firstResult
    evalBinary operator firstArg secondArg = do
      firstResult  <- evalM firstArg action
      secondResult <- evalM secondArg action
      action $ operator firstResult secondResult

-- | Evaluates expression into custom State.
evalS :: Expr -> State [Prim Double] Double
evalS expr = evalM expr $ \evaluatedOperation -> do
  modifyState $ (:) evaluatedOperation
  return $ calculate evaluatedOperation

-- | Well-named version of evalS.
eval :: Expr -> State [Prim Double] Double
eval = evalS
