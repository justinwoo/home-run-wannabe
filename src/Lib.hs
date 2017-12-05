{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Lib where

import Data.Either.Validation
import Data.Functor.Const
import Data.Generics.Product
import Data.Proxy
import Data.Semigroup
import GHC.Generics
import GHC.TypeLits

-- GRowToList from kcsongor
type family GRowToList (r :: * -> *) :: [(Symbol, *)] where
  GRowToList (l :*: r)
    = GRowToList l ++ GRowToList r
  GRowToList (S1 ('MetaSel ('Just name) _ _ _) (Rec0 a))
    = '[ '(name, a) ]
  GRowToList (M1 _ m a)
    = GRowToList a
  GRowToList U1 = '[]

type family (a :: [k]) ++ (b :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

-- Validate Rule, where a given rule can also be used to validate any value type
class ValidateRule rule a where
  validateRuleImpl :: a -> Bool

-- Validated Structure
type ValidatedValue rules value = Const value (Proxy rules)

-- Check the validations defined
class CheckRules (rulesL :: [(Symbol, *)]) a where
  checkRulesImpl :: a -> Validation [String] ()

instance CheckRules '[] a where
  checkRulesImpl _ = pure ()

instance
  ( KnownSymbol name
  , ValidateRule rule a
  , CheckRules tail a
  ) => CheckRules ('(name, rule) ': tail) a where
  checkRulesImpl x = (<>) <$> curr <*> rest
    where
      curr = if validateRuleImpl @rule x
        then pure ()
        else Failure . pure $ symbolVal @name Proxy
      rest = const () <$> checkRulesImpl @tail x

-- exposed function
checkRules :: forall a rules rulesL
   . Generic rules
  => rulesL ~ GRowToList (Rep rules)
  => CheckRules rulesL a
  => Proxy rules
  -> a
  -> Validation [String] (ValidatedValue rules a)
checkRules _ a =
  const (Const a) <$> checkRulesImpl @rulesL a
