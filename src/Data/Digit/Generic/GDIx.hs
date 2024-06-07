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

import GHC.TypeNats (Nat)

type role GDIx phantom phantom representational

-- | A generic type representing a digit index.
newtype GDIx (neg :: Nat) (pos :: Nat) int
  = -- | A digit index with a value of type @int@.
    GDIx int
  deriving (Eq, Ord, Num, Real, Enum, Integral)
