module HW2.T5
  ( ExceptState(..)
  , eval
  , evalES
  , joinExceptState
  , mapExceptState
  , modifyExceptState
  , throwExceptState
  , wrapExceptState
  ) where

import           Control.Monad  (ap)
import           Data.Bifunctor (bimap)
import           HW2.T1
import           HW2.T2         (distAnnotated, distExcept)
import           HW2.T4         (BinaryOperation, Expr (..), Prim (..),
                                 UnaryOperation, calculate)

-- | Custom implementation of exceptionable state.
data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

-- | Maps values in custom exceptionable state.
mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f ES { runES = exceptor } = ES { runES = mapExcept (mapAnnotated f) . exceptor }

-- | Wraps custom exceptionable state over value.
wrapExceptState :: a -> ExceptState e s a
wrapExceptState value = ES { runES = Success . (:#) value }

-- | Extracts custom annotated value from nested custom exceptionable state in annotated value.
extractExcept :: Except e (Annotated s (ExceptState e s a)) -> Except e (Annotated s a)
extractExcept (Error error')                  = Error error'
extractExcept (Success (state :# annotation)) = runES state annotation

-- | Joins nested custom exceptionable states.
joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState ES { runES = exceptor } = ES { runES = extractExcept . exceptor }

-- | Creates instance of custom exceptionable state with modifying function for annotation.
modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState modifier = ES { runES = Success . (:#) () . modifier }

throwExceptState :: e -> ExceptState e s a
throwExceptState error' = ES { runES = const $ Error error' }

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

-- | Custom implementation of evaluation error.
data EvaluationError = DivideByZero
  deriving Show

-- | Result for exceptionable evaluation state.
type EvaluationResult = Except EvaluationError (Annotated [Prim Double] (Prim Double))

-- | Gets evaluation result from custom unary operation and argument.
getUnaryEval :: UnaryOperation -> Expr -> EvaluationResult
getUnaryEval op = mapExcept (mapAnnotated op) . getEval

-- | Gets evaluation result from custom binary operation and arguments.
getBinaryEval :: BinaryOperation -> (Expr, Expr) -> EvaluationResult
getBinaryEval op = (mapExcept flatMapper . distExcept) . bimap getEval getEval
  where
    flatMapper = mapAnnotated (uncurry op) . distAnnotated

-- | Gets fully calculated version of evaluation result for any expression.
getEval :: Expr -> Except EvaluationError (Annotated [Prim Double] Double)
getEval arg = runES (eval arg) mempty

-- | Evaluates expression into exceptionable state.
eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val value) = pure value
eval (Op operation) = do
  let wrapped = case operation of
        (Add x y) -> getBinaryEval Add (x, y)
        (Sub x y) -> getBinaryEval Sub (x, y)
        (Mul x y) -> getBinaryEval Mul (x, y)
        (Div x y) -> getBinaryEval Div (x, y)
        (Abs x)   -> getUnaryEval Abs x
        (Sgn x)   -> getUnaryEval Sgn x
  case wrapped of
    Error error'                 -> throwExceptState error'
    Success (Div _ 0 :# _)       -> throwExceptState DivideByZero
    Success (prim :# annotation) -> calculate prim <$ modifyExceptState ((<>) (prim : annotation))

-- | Well-named version of eval.
evalES :: Expr -> ExceptState EvaluationError [Prim Double] Double
evalES = eval
