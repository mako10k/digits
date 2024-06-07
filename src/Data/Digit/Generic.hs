{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Digit.Generic
  ( GenDigit (),
    pattern GenDigit,
  )
where

import Data.Bifunctor (Bifunctor (first, second))
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
import Data.Digit.Generic.GDIx
import Data.Function (fix, on)
import GHC.TypeNats (KnownNat)

-- | Represents a generic digit type with negative bound and positive bound.
data GenDigit neg pos int where
  -- | A digit in the range from @-neg@ to @pos@. (Do not use this constructor directly, use 'GenDigit' instead.)
  UnsafeGenDigit :: (KnownNat neg, KnownNat pos, Integral int) => StrictGDIx neg pos int -> GenDigit neg pos int

-- | A pattern synonym for 'GenDigit'.
pattern GenDigit :: LenientGDIx neg pos int -> GenDigit neg pos int
pattern GenDigit ix <- UnsafeGenDigit (convertGDIx -> ix)
  where
    GenDigit = UnsafeGenDigit . convertGDIx

{-# COMPLETE GenDigit #-}

-- | Check if an integer is in the range of a 'GenDigit'.
instance (KnownNat neg, KnownNat pos, Integral int) => Show (GenDigit neg pos int) where
  show (GenDigit ix) = toCharGDIx ix 
    Just ch -> [ch]
    _ -> "(" ++ show (toInteger ix) ++ ")"
  showList [] = showString "0"
  showList xs = fix go xs
    where
      go _ [] = id
      go f (y : ys) = f ys . shows y

instance (KnownNat neg, KnownNat pos, Integral int) => Read (GenDigit neg pos int) where
  readsPrec _ [] = []
  readsPrec _ ('0' : xs) = [(0, xs)]
  readsPrec _ ((fromCharGDIx -> Just ix) : xs) = [(GenDigit ix, xs)]
  readsPrec d xs = first GenDigit <$> readsPrec d xs

instance (KnownNat neg, KnownNat pos, Integral int) => Digit (GenDigit neg pos int) where
  fromDigit (GenDigit ix) = fromIntegral ix
  toDigit int = GenDigit $ StrictGDIx $ (int - fromGDIx minGDIx) `mod` fromGDIx sizeGDIx + fromGDIx minGDIx
  digitMin = GenDigit minGDIx
  digitMax = GenDigit maxGDIx

instance (KnownNat neg, KnownNat pos, Integral int) => Num (GenDigit neg pos int) where
  GenDigit x + GenDigit y = GenDigit $ x + y
  GenDigit x * GenDigit y = GenDigit $ x * y
  GenDigit x - GenDigit y = GenDigit $ x - y
  abs (GenDigit x) = GenDigit $ abs x
  signum (GenDigit x) = GenDigit $ signum x
  fromInteger = GenDigit . fromInteger

onWithCarry :: forall ix neg pos int. (GDIx ix neg pos int) => (ix neg pos int -> ix neg pos int -> ix neg pos int) -> ix neg pos int -> ix neg pos int -> (ix neg pos int, ix neg pos int)
onWithCarry (|?|) x y = second (minGDIx +) ((x |?| y - minGDIx) `divMod` sizeGDIx)

instance (KnownNat pos, KnownNat neg, Integral int) => Num [GenDigit pos neg int] where
  (+) = f 0
    where
      f c [] []
        | c == 0 = 0
        | otherwise = [GenDigit c]
      f c [] ys = f c [0] ys
      f c xs [] = f c xs [0]
      f c (GenDigit x : xs) (GenDigit y : ys) = GenDigit z2 : f (c1 + c2) xs ys
        where
          (c1, z1) = onWithCarry (+) x y
          (c2, z2) = onWithCarry (+) z1 c

  xs * ys = fromInteger $ on (*) fromDigits xs ys

  (-) = f 0
    where
      f c [] []
        | c == 0 = 0
        | otherwise = [GenDigit c]
      f c [] ys = f c [0] ys
      f c xs [] = f c xs [0]
      f c (GenDigit x : xs) (GenDigit y : ys) = GenDigit z2 : f (c1 + c2) xs ys
        where
          (c1, z1) = onWithCarry (-) x y
          (c2, z2) = onWithCarry (-) z1 c
  abs = fromInteger . abs . fromDigits
  signum = fromInteger . signum . fromDigits
  fromInteger = toDigits