{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Data.Digit.Generic.GDIx
-- Description : Module for generic digit operations using Index type
module Data.Digit.Generic.GDIx
  ( -- * GDIx type
    LGDIx (LGDIx),
    StrictGDIx (),
    pattern StrictGDIx,
    GDIx (..),
  )
where

import Control.Applicative (Alternative (empty))
import Control.Monad (guard)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Coerce (coerce)
import Data.Data (Proxy (Proxy))
import Data.Function (on)
import GHC.TypeNats (KnownNat, Nat, natVal)

class (KnownNat neg, KnownNat pos, Integral int) => GDIx ix neg pos int where
  fromGDIx :: ix neg pos int -> int
  toGDIx :: int -> ix neg pos int
  minGDIx :: ix neg pos int
  minGDIx = toGDIx (-fromIntegral (natVal (Proxy @neg)))
  maxGDIx :: ix neg pos int
  maxGDIx = toGDIx (fromIntegral (natVal (Proxy @pos)))
  sizeGDIx :: ix neg pos int
  sizeGDIx = toGDIx (fromIntegral (natVal (Proxy @pos)) - fromIntegral (natVal (Proxy @neg)) + 1)
  convertGDIx :: (GDIx ix' neg pos int) => ix neg pos int -> ix' neg pos int
  convertGDIx = toGDIx . fromGDIx
  idSafeGDIx :: (Alternative t) => ix neg pos int -> t (ix neg pos int)
  idSafeGDIx ix = ix <$ guard (minGDIx <?= ix && ix <?= maxGDIx)
    where
      (<?=) = on (<=) fromGDIx
  toSafeGDIx :: (Alternative t) => int -> t (ix neg pos int)
  toSafeGDIx int = toGDIx int <$ guard (fromGDIx' minGDIx <= int && int <= fromGDIx' maxGDIx)
    where
      fromGDIx' = fromGDIx :: ix neg pos int -> int
  toCharGDIx :: (Alternative t) => ix neg pos int -> t Char
  toCharGDIx = toCharGDIx' . toGDIxSafe' . fromGDIx
    where
      toCharGDIx' (Just (LGDIx int))
        | 0 <= int && int <= 9 = pure $ toEnum $ 48 + fromIntegral int
        | 10 <= int && int <= 36 = pure $ toEnum $ 55 + fromIntegral int
        | -26 <= int && int <= -1 = pure $ toEnum $ 96 - fromIntegral int
      toCharGDIx' _ = empty
      toGDIxSafe' :: (Alternative t) => int -> t (LGDIx neg pos int)
      toGDIxSafe' = toSafeGDIx
  fromCharGDIx :: (Alternative t) => Char -> t (ix neg pos int)
  fromCharGDIx ch
    | isDigit ch = toSafeGDIx $ fromIntegral $ fromEnum ch - 48
    | isAsciiUpper ch = toSafeGDIx $ fromIntegral $ fromEnum ch - 55
    | isAsciiLower ch = toSafeGDIx $ fromIntegral $ 96 - fromEnum ch
    | otherwise = empty

type role LGDIx phantom phantom representational

-- | A generic type representing a digit index.
newtype LGDIx (neg :: Nat) (pos :: Nat) int
  = -- | A digit index with a value of type @int@.
    LGDIx int
  deriving (Eq, Ord, Num, Real, Enum, Integral)

instance (KnownNat neg, KnownNat pos, Integral int) => GDIx LGDIx neg pos int where
  fromGDIx = coerce
  toGDIx = coerce

instance (KnownNat neg, KnownNat pos, Integral int) => Read (LGDIx neg pos int) where
  readsPrec d s = [(fromInteger int, t) | (int, t) <- readsPrec d s]

instance (KnownNat neg, KnownNat pos, Integral int) => Show (LGDIx neg pos int) where
  showsPrec d ix = showsPrec d (toInteger ix)

instance (KnownNat neg, KnownNat pos, Integral int) => Read (StrictGDIx neg pos int) where
  readsPrec d s = [(ix, t) | (int, t) <- readsPrec d s, ix <- toSafeGDIx (fromInteger int)]

instance (KnownNat neg, KnownNat pos, Integral int) => Show (StrictGDIx neg pos int) where
  showsPrec d ix = showsPrec d (toInteger ix)

type role StrictGDIx phantom phantom representational

newtype StrictGDIx (neg :: Nat) (pos :: Nat) int
  = UnsafeStrictGDIx int
  deriving (Eq, Ord, Num, Real, Enum, Integral)

pattern StrictGDIx :: forall neg pos int. (KnownNat neg, KnownNat pos, Integral int) => int -> StrictGDIx neg pos int
pattern StrictGDIx int <- (coerce -> int)
  where
    StrictGDIx = toGDIx

{-# COMPLETE StrictGDIx #-}

instance (KnownNat neg, KnownNat pos, Integral int) => GDIx StrictGDIx neg pos int where
  fromGDIx = coerce
  toGDIx int = case toSafeGDIx int of
    Just ix -> ix
    Nothing -> error "StrictGDIx: out of range"

instance (KnownNat neg, KnownNat pos, Integral int) => Bounded (StrictGDIx neg pos int) where
  minBound = minGDIx
  maxBound = maxGDIx