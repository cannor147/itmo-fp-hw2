module HW2.T2
  ( distAnnotated
  , distExcept
  , distFun
  , distList
  , distOption
  , distPair
  , distPrioritised
  , distQuad
  , distStream
  , wrapAnnotated
  , wrapExcept
  , wrapFun
  , wrapList
  , wrapOption
  , wrapPair
  , wrapPrioritised
  , wrapQuad
  , wrapStream
  ) where

import           HW2.T1

-- | Distributivity of custom optional over pair.
distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _)                           = None
distOption (_, None)                           = None
distOption (Some firstValue, Some secondValue) = Some (firstValue, secondValue)

-- | Distributivity of custom pair over pair.
distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P x1 y1, P x2 y2) = P (x1, x2) (y1, y2)

-- | Distributivity of custom quad over pair.
distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q x1 y1 z1 w1, Q x2 y2 z2 w2) = Q (x1, x2) (y1, y2) (z1, z2) (w1, w2)

-- | Distributivity of custom annotated value over pair.
distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (firstValue :# firstAnnotation, secondValue :# secondAnnotation) =
  (firstValue, secondValue) :# firstAnnotation <> secondAnnotation

-- | Distributivity of custom exception either over pair.
distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error firstError, _)                     = Error firstError
distExcept (_, Error secondError)                    = Error secondError
distExcept (Success firstValue, Success secondValue) = Success (firstValue, secondValue)

-- | Distributivity of custom prioritised value over pair.
distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low firstValue, Low secondValue)       = Low (firstValue, secondValue)
distPrioritised (Low firstValue, Medium secondValue)    = Medium (firstValue, secondValue)
distPrioritised (Low firstValue, High secondValue)      = High (firstValue, secondValue)
distPrioritised (Medium firstValue, Low secondValue)    = Medium (firstValue, secondValue)
distPrioritised (Medium firstValue, Medium secondValue) = Medium (firstValue, secondValue)
distPrioritised (Medium firstValue, High secondValue)   = High (firstValue, secondValue)
distPrioritised (High firstValue, Low secondValue)      = High (firstValue, secondValue)
distPrioritised (High firstValue, Medium secondValue)   = High (firstValue, secondValue)
distPrioritised (High firstValue, High secondValue)     = High (firstValue, secondValue)

-- | Distributivity of custom stream over pair.
distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (firstHead :> firstTail, secondHead :> secondTail) =
  (firstHead, secondHead) :> distStream (firstTail, secondTail)

-- | Distributivity of custom list over pair.
distList :: (List a, List b) -> List (a, b)
distList (Nil, _)                         = Nil
distList (_, Nil)                         = Nil
distList (firstHead :. firstTail, second) = pairLine second
  where
    pairLine (head' :. tail') = (firstHead, head') :. pairLine tail'
    pairLine Nil              = distList (firstTail, second)

-- | Distributivity of custom function object over pair.
distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F firstFunction, F secondFunction) = F (\x -> (firstFunction x, secondFunction x))

-- | Wraps custom optional over value.
wrapOption :: a -> Option a
wrapOption value = Some value

-- | Wraps custom pair over value.
wrapPair :: a -> Pair a
wrapPair value = P value value

-- | Wraps custom quad over value.
wrapQuad :: a -> Quad a
wrapQuad value = Q value value value value

-- | Annotates value.
wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated value = value :# mempty

-- | Wraps custom exception either over value.
wrapExcept :: a -> Except e a
wrapExcept value = Success value

-- | Prioritized value.
wrapPrioritised :: a -> Prioritised a
wrapPrioritised value = Low value

-- | Wraps custom stream over value.
wrapStream :: a -> Stream a
wrapStream value = value :> wrapStream value

-- | Wraps custom list over value.
wrapList :: a -> List a
wrapList value = value :. Nil

-- | Creates constant function object from value.
wrapFun :: a -> Fun i a
wrapFun value = F (const value)
