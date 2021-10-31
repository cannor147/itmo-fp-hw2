module HW2.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import           HW2.T1

joinOption :: Option (Option a) -> Option a
joinOption None         = None
joinOption (Some value) = value

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error error')  = Error error'
joinExcept (Success value) = value

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((value :# innerAnnotation) :# outerAnnotation) =
  value :# innerAnnotation <> outerAnnotation

joinList :: List (List a) -> List a
joinList Nil              = Nil
joinList (head' :. tail') = joinLine head'
  where
    joinLine (head'' :. tail'') = head'' :. joinLine tail''
    joinLine Nil                = joinList tail'

extractFun :: Fun i a -> (i -> a)
extractFun (F function) = function

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F outerFunction) = F $ \x -> extractFun (outerFunction x) x
