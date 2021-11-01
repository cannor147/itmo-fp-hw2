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

distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _)                           = None
distOption (_, None)                           = None
distOption (Some firstValue, Some secondValue) = Some (firstValue, secondValue)

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P x1 y1, P x2 y2) = P (x1, x2) (y1, y2)

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q x1 y1 z1 w1, Q x2 y2 z2 w2) = Q (x1, x2) (y1, y2) (z1, z2) (w1, w2)

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (firstValue :# firstAnnotation, secondValue :# secondAnnotation) =
  (firstValue, secondValue) :# firstAnnotation <> secondAnnotation

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error firstError, _)                     = Error firstError
distExcept (_, Error secondError)                    = Error secondError
distExcept (Success firstValue, Success secondValue) = Success (firstValue, secondValue)

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

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (firstHead :> firstTail, secondHead :> secondTail) =
  (firstHead, secondHead) :> distStream (firstTail, secondTail)

distList :: (List a, List b) -> List (a, b)
distList (Nil, _)                         = Nil
distList (_, Nil)                         = Nil
distList (firstHead :. firstTail, second) = pairLine second
  where
    pairLine (head' :. tail') = (firstHead, head') :. pairLine tail'
    pairLine Nil              = distList (firstTail, second)

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F firstFunction, F secondFunction) = F (\x -> (firstFunction x, secondFunction x))

wrapOption :: a -> Option a
wrapOption value = Some value

wrapPair :: a -> Pair a
wrapPair value = P value value

wrapQuad :: a -> Quad a
wrapQuad value = Q value value value value

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated value = value :# mempty

wrapExcept :: a -> Except e a
wrapExcept value = Success value

wrapPrioritised :: a -> Prioritised a
wrapPrioritised value = Low value

wrapStream :: a -> Stream a
wrapStream value = value :> wrapStream value

wrapList :: a -> List a
wrapList value = value :. Nil

wrapFun :: a -> Fun i a
wrapFun value = F (const value)
