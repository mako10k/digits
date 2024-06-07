{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Digit.Generic.GDIx
-- Description : Module for generic digit operations using Index type
module Data.Digit.Generic.GDIx
  ( -- * GDIx type
    GDIx (GDIx),
  )
where

import Data.Bifunctor (Bifunctor (first))
import GHC.TypeNats (Nat)

type role GDIx phantom phantom representational

-- | A generic type representing a digit index.
newtype GDIx (neg :: Nat) (pos :: Nat) int
  = -- | A digit index with a value of type @int@.
    GDIx int
  deriving (Eq, Ord, Num, Real, Enum, Integral, Functor)

instance (Show int) => Show (GDIx neg pos int) where
  show (GDIx i) = show i

instance (Read int) => Read (GDIx neg pos int) where
  readsPrec i s = first GDIx <$> readsPrec i s

instance Applicative (GDIx neg pos) where
  pure = GDIx
  GDIx f <*> GDIx x = GDIx (f x)

instance Monad (GDIx neg pos) where
  GDIx x >>= f = f x
  return = pure