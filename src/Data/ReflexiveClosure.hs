{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Data.ReflexiveClosure where

import Control.Category
import Control.Lens.Prism
import Control.Semigroupoid (Semigroupoid)
import qualified Control.Semigroupoid as S

data ReflexiveClosure κ a b where
    Refl :: ReflexiveClosure κ a a
    Lift :: κ a b -> ReflexiveClosure κ a b

instance Semigroupoid κ => Category (ReflexiveClosure κ) where
    id = Refl
    Refl . y = y
    x . Refl = x
    Lift x . Lift y = Lift (x S.. y)

liftL :: Prism (ReflexiveClosure κ a b) (ReflexiveClosure κ' a b) (κ a b) (κ' a b)
liftL = prism Lift \ case
    Refl -> Left Refl
    Lift κ -> Right κ
