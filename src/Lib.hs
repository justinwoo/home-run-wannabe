{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
  validateRuleImpl :: Proxy rule -> a -> Bool

-- Validated Structure
type ValidatedValue rules value = Const value (Proxy rules)

-- Check the validations defined
class CheckRules (rulesL :: [(Symbol, *)]) rules a where
  checkRulesImpl :: Proxy rulesL -> Proxy rules -> a -> Validation [String] ()

instance CheckRules '[] rules a where
  checkRulesImpl _ _ _ = pure ()

instance
  ( KnownSymbol name
  , ValidateRule rule a
  , HasField name rule rules
  , CheckRules tail rules a
  ) => CheckRules ('(name, rule) ': tail) rules a where
  checkRulesImpl _ rulesP x = (<>) <$> curr <*> rest
    where
      curr = if validateRuleImpl (Proxy @rule) x
        then pure ()
        else Failure . pure $ symbolVal (Proxy @name)
      rest = const () <$> checkRulesImpl (Proxy @tail) rulesP x

-- exposed function
checkRules :: forall a rules rulesL
   . Generic rules
  => rulesL ~ GRowToList (Rep rules)
  => CheckRules rulesL rules a
  => Proxy rules
  -> a
  -> Validation [String] (ValidatedValue rules a)
checkRules rulesP a =
  const (Const a) <$> checkRulesImpl (Proxy @rulesL) rulesP a