{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Digit.Generic
  ( GenDigit (),
    pattern GenDigit,
    pattern GenDigitMod,
    pattern GenDigitIx,
  )
where

import Data.Bifunctor (Bifunctor (first, second))
import Data.Char (isDigit, isLower, isUpper)
import Data.Coerce (coerce)
import Data.Data (Proxy (Proxy))
import Data.Digit
  ( Digit
      ( digitMax,
        digitMin,
        fromDigit,
        fromDigits,
        toDigit,
        toDigits
      ),
  )
import Data.Digit.Generic.GDIx (GDIx (GDIx))
import Data.Function (on)
import GHC.TypeNats (KnownNat, natVal)

-- | Represents a generic digit type with negative bound and positive bound.
data GenDigit neg pos int where
  -- | A digit in the range from @-neg@ to @pos@. (Do not use this constructor directly, use 'GenDigit' instead.)
  UnsafeGenDigit :: int -> GenDigit neg pos int

pattern GenDigit :: forall neg pos int. (KnownNat neg, KnownNat pos, Integral int) => int -> GenDigit neg pos int
pattern GenDigit int <- UnsafeGenDigit int
  where
    GenDigit int | neg <= int && int <= pos = UnsafeGenDigit int
      where
        neg = -fromIntegral (natVal (Proxy @neg))
        pos = fromIntegral (natVal (Proxy @pos))
    GenDigit _ = error "GenDigit: index out of bounds"

{-# COMPLETE GenDigit #-}

pattern GenDigitMod :: forall neg pos int. (KnownNat neg, KnownNat pos, Integral int) => int -> GenDigit neg pos int
pattern GenDigitMod int <- UnsafeGenDigit int
  where
    GenDigitMod int = UnsafeGenDigit ((int - neg) `mod` (pos - neg + 1) + neg)
      where
        neg = -fromIntegral (natVal (Proxy @neg))
        pos = fromIntegral (natVal (Proxy @pos))

{-# COMPLETE GenDigitMod #-}

pattern GenDigitIx :: forall neg pos int. (KnownNat neg, KnownNat pos, Integral int) => GDIx neg pos int -> GenDigit neg pos int
pattern GenDigitIx ix <- GenDigit (coerce -> ix)
  where
    GenDigitIx = GenDigit . coerce

{-# COMPLETE GenDigitIx #-}

-- | Check if an integer is in the range of a 'GenDigit'.
instance (KnownNat neg, KnownNat pos, Integral int) => Show (GenDigit neg pos int) where
  show (GenDigit int)
    | 0 <= int && int <= 9 = [toEnum (fromIntegral int + fromEnum '0')]
    | 10 <= int && int <= 35 = [toEnum (fromIntegral int - 10 + fromEnum 'A')]
    | -26 <= int && int < -1 = [toEnum (-fromIntegral int - 1 + fromEnum 'a')]
    | otherwise = "(" ++ show (toInteger int) ++ ")"
  showList [] = showString "0"
  showList xs = showString (concatMap show (reverse xs))

instance (KnownNat neg, KnownNat pos, Integral int) => Read (GenDigit neg pos int) where
  readsPrec _ [] = []
  readsPrec _ (x : xs)
    | isDigit x = [(GenDigit (fromIntegral (fromEnum x - fromEnum '0')), xs)]
    | isUpper x = [(GenDigit (fromIntegral (fromEnum x - fromEnum 'A' + 10)), xs)]
    | isLower x = [(GenDigit (fromIntegral (-fromEnum x + fromEnum 'a' - 1)), xs)]
  readsPrec d xs = first (GenDigit . fromInteger) <$> readsPrec d xs

instance (KnownNat neg, KnownNat pos, Integral int) => Digit (GenDigit neg pos int) where
  fromDigit (GenDigit int) = fromIntegral int
  toDigit int = GenDigit (fromIntegral int)
  digitMin = GenDigit (-fromIntegral (natVal (Proxy @neg)))
  digitMax = GenDigit (fromIntegral (natVal (Proxy @pos)))

onWithCarry :: forall neg pos int. (KnownNat neg, KnownNat pos, Integral int) => (GDIx neg pos int -> GDIx neg pos int -> GDIx neg pos int) -> GDIx neg pos int -> GDIx neg pos int -> (GDIx neg pos int, GDIx neg pos int)
onWithCarry (|?|) x y = second (neg +) ((x |?| y - neg) `divMod` (pos - neg + 1))
  where
    neg = -fromIntegral (natVal (Proxy @neg))
    pos = fromIntegral (natVal (Proxy @pos))

instance (KnownNat pos, KnownNat neg, Integral int) => Num [GenDigit pos neg int] where
  (+) = go 0
    where
      go cix [] []
        | cix == 0 = 0
        | otherwise = [GenDigitIx cix]
      go cix [] ys = go cix [GenDigitIx 0] ys
      go cix xs [] = go cix xs [GenDigitIx 0]
      go cix (GenDigitIx x : xs) (GenDigitIx y : ys) = GenDigitIx z2 : go (cix1 + cix2) xs ys
        where
          (cix1, z1) = onWithCarry (+) x y
          (cix2, z2) = onWithCarry (+) z1 cix

  xs * ys = fromInteger $ on (*) fromDigits xs ys

  (-) = go 0
    where
      go cix [] []
        | cix == 0 = 0
        | otherwise = [GenDigitIx cix]
      go cix [] ys = go cix [GenDigitIx 0] ys
      go cix xs [] = go cix xs [GenDigitIx 0]
      go cix (GenDigitIx x : xs) (GenDigitIx y : ys) = GenDigitIx z2 : go (cix1 + cix2) xs ys
        where
          (cix1, z1) = onWithCarry (-) x y
          (cix2, z2) = onWithCarry (-) z1 cix
  abs = fromInteger . abs . fromDigits
  signum = fromInteger . signum . fromDigits
  fromInteger = toDigits