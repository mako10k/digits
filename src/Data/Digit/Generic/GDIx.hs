{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Data.Digit.Generic.GDIx
-- Description : Module for generic digit operations using Index type
module Data.Digit.Generic.GDIx
  ( -- * GDIx type
    GDIx (GDIx),

    -- * Conversion functions
    gdIxToIntegral,
    integralToGDIx,

    -- * check if GDIx has proper index
    isProperGDIx,

    -- * GDIx bounds and size
    minGDIx,
    maxGDIx,
    sizeGDIx,

    -- * GDIx and Char conversion
    gdIxToChar,
    charToGDIx,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Data (Proxy (Proxy))
import GHC.TypeNats (KnownNat, natVal)

type role GDIx phantom phantom representational

-- | A generic type representing a digit index.
newtype GDIx neg pos int
  = -- | A digit index with a value of type @int@.
    GDIx int
  deriving (Eq, Ord, Num, Real, Enum, Integral)

-- | Convert an integral value to a 'GDIx' value.
integralToGDIx :: (Integral int, Integral int') => int -> GDIx neg pos int'
integralToGDIx = GDIx . fromIntegral

-- | Convert a 'GDIx' value to an integral value.
gdIxToIntegral :: (Integral int, Integral int') => GDIx neg pos int -> int'
gdIxToIntegral (GDIx ix) = fromIntegral ix

-- | Check if a 'GDIx' value has a proper index.
isProperGDIx :: forall neg pos int. (KnownNat neg, KnownNat pos, Integral int) => GDIx neg pos int -> Bool
isProperGDIx ix = minGDIx <= ix && ix <= maxGDIx

-- | Get the minimum 'GDIx' value.
minGDIx :: forall neg pos int. (KnownNat neg, Integral int) => GDIx neg pos int
minGDIx = GDIx (-fromIntegral (natVal (Proxy @neg)))

-- | Get the maximum 'GDIx' value.
maxGDIx :: forall neg pos int. (KnownNat pos, Integral int) => GDIx neg pos int
maxGDIx = GDIx (fromIntegral (natVal (Proxy @pos)))

-- | Get the size of 'GDIx'.
sizeGDIx :: forall neg pos int. (KnownNat neg, KnownNat pos, Integral int) => GDIx neg pos int
sizeGDIx = maxGDIx - minGDIx + 1

instance (KnownNat neg, KnownNat pos, Integral int) => Read (GDIx neg pos int) where
  readsPrec d s = do
    (int :: Int, t) <- readsPrec d s
    let ix :: GDIx neg pos int = GDIx (fromIntegral int)
    True <- return $ isProperGDIx ix
    return (ix, t)

-- | Convert a 'GDIx' value to a 'Char' value.
gdIxToChar :: (Integral int) => GDIx neg pos int -> Maybe Char
gdIxToChar digit = foldl1 (<|>) (map ($ digit) [gdIxToCharDigit, gdIxToCharAsciiUpper, gdIxToCharAsciiLower])

-- | Convert a 'Char' value to a 'GDIx' value (only for digit characters)
gdIxToCharDigit :: (Integral int) => GDIx neg pos int -> Maybe Char
gdIxToCharDigit ix | 0 <= ix && ix <= 9 = Just $ toEnum $ fromEnum '0' + fromIntegral ix
gdIxToCharDigit _ = Nothing

-- | Convert a 'Char' value to a 'GDIx' value (only for ASCII uppercase characters)
gdIxToCharAsciiUpper :: (Integral int) => GDIx neg pos int -> Maybe Char
gdIxToCharAsciiUpper ix | 10 <= ix && ix <= 36 = Just $ toEnum $ fromEnum 'A' + fromIntegral ix - 10
gdIxToCharAsciiUpper _ = Nothing

-- | Convert a 'Char' value to a 'GDIx' value (only for ASCII lowercase characters)
gdIxToCharAsciiLower :: (Integral int) => GDIx neg pos int -> Maybe Char
gdIxToCharAsciiLower ix | -26 <= ix && ix <= -1 = Just $ toEnum $ fromEnum 'a' - fromIntegral ix - 1
gdIxToCharAsciiLower _ = Nothing

-- | Convert a 'Char' value to a 'GDIx' value (only for digit characters)
charDigitToGDIx :: (KnownNat pos, Integral int) => Char -> Maybe (GDIx neg pos int)
charDigitToGDIx ch
  | isDigit ch && ix <= maxGDIx = Just ix
  | otherwise = Nothing
  where
    ix = integralToGDIx $ fromEnum ch - fromEnum '0'

-- | Convert a 'Char' value to a 'GDIx' value (only for ASCII uppercase characters)
charAsciiUpperToGDIx :: (KnownNat pos, Integral int) => Char -> Maybe (GDIx neg pos int)
charAsciiUpperToGDIx ch
  | isAsciiUpper ch && ix <= maxGDIx = Just ix
  | otherwise = Nothing
  where
    ix = integralToGDIx $ fromEnum ch - fromEnum 'A' + 10

-- | Convert a 'Char' value to a 'GDIx' value (only for ASCII lowercase characters)
charAsciiLowerToGDIx :: (KnownNat neg, Integral int) => Char -> Maybe (GDIx neg pos int)
charAsciiLowerToGDIx ch
  | isAsciiLower ch && ix > minGDIx = Just ix
  | otherwise = Nothing
  where
    ix = integralToGDIx $ fromEnum 'a' - fromEnum ch - 1

-- | Convert a 'Char' value to a 'GDIx' value
charToGDIx :: forall neg pos int. (KnownNat neg, KnownNat pos, Integral int) => Char -> Maybe (GDIx neg pos int)
charToGDIx (charDigitToGDIx -> Just ix) = Just ix
charToGDIx (charAsciiUpperToGDIx -> Just ix) = Just ix
charToGDIx (charAsciiLowerToGDIx -> Just ix) = Just ix
charToGDIx _ = Nothing
