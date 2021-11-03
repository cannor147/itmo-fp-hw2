module HW2.T3
  ( joinAnnotated
  , joinExcept
  , joinFun
  , joinList
  , joinOption
  ) where

import           HW2.T1

-- | Joins nested custom optionals.
joinOption :: Option (Option a) -> Option a
joinOption None         = None
joinOption (Some value) = value

-- | Joins nested custom exception eithers.
joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error error')  = Error error'
joinExcept (Success value) = value

-- | Joins nested custom annotated values.
joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((value :# innerAnnotation) :# outerAnnotation) =
  value :# innerAnnotation <> outerAnnotation

-- | Joins nested custom list.
joinList :: List (List a) -> List a
joinList Nil              = Nil
joinList (head' :. tail') = joinLine head'
  where
    joinLine (head'' :. tail'') = head'' :. joinLine tail''
    joinLine Nil                = joinList tail'

-- | Extracts function from custom function object.
extractFun :: Fun i a -> (i -> a)
extractFun (F function) = function

-- | Joins nested function objects.
joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F outerFunction) = F $ \x -> extractFun (outerFunction x) x
