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
import           HW2.T1
import           HW2.T4         (Expr (..), Prim (..), calculate, evalM)

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

-- | Evaluates expression into custom ExceptState.
evalES :: Expr -> ExceptState EvaluationError [Prim Double] Double
evalES expr = evalM expr $ \evaluatedOperation -> case evaluatedOperation of
  (Div _ 0) -> throwExceptState DivideByZero
  _         -> do
    modifyExceptState $ (:) evaluatedOperation
    return $ calculate evaluatedOperation

-- | Well-named version of evalES.
eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval = evalES
