{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Digit.Generic
  ( GenDigit (),
    pattern GenDigit,
    genDigitToChar,
    charToGenDigit,
    toGenDigit,
    fromGenDigit,
  )
where

import Data.Bifunctor (Bifunctor (second))
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
  ( GDIx,
    charToGDIx,
    gdIxToChar,
    gdIxToIntegral,
    integralToGDIx,
    isProperGDIx,
    maxGDIx,
    minGDIx,
    sizeGDIx,
  )
import Data.Function (fix, on)
import GHC.TypeNats (KnownNat)

data GenDigit neg pos int where
  -- | A digit in the range from @-neg@ to @pos@. (Do not use this constructor directly, use 'GenDigit' instead.)
  UnsafeGenDigit :: GDIx neg pos int -> GenDigit neg pos int

pattern GenDigit :: forall neg pos int. (KnownNat neg, KnownNat pos, Integral int) => GDIx neg pos int -> GenDigit neg pos int
pattern GenDigit ix <- UnsafeGenDigit ix
  where
    GenDigit = toGenDigit

{-# COMPLETE GenDigit #-}

toGenDigit :: forall neg pos int. (KnownNat neg, KnownNat pos, Integral int) => GDIx neg pos int -> GenDigit neg pos int
toGenDigit ix
  | isProperGDIx ix = UnsafeGenDigit ix
  | otherwise = error "GenDigit: out of range"

fromGenDigit :: GenDigit neg pos int -> GDIx neg pos int
fromGenDigit (UnsafeGenDigit ix) = ix

-- | Check if an integer is in the range of a 'GenDigit'.
instance (KnownNat neg, KnownNat pos, Integral int) => Show (GenDigit neg pos int) where
  show (GenDigit ix)
    | 0 <= ix && ix <= 9 = show (toInteger ix)
    | 10 <= ix && ix <= 36 = [toEnum $ fromEnum 'A' + fromIntegral ix - 10]
    | -26 <= ix && ix <= -1 = [toEnum $ fromEnum 'a' - fromIntegral ix - 1]
    | otherwise = "(" ++ show (toInteger ix) ++ ")"
  showList [] = showString "0"
  showList xs = fix go xs
    where
      go _ [] = id
      go f (y : ys) = f ys . shows y

genDigitToChar :: (KnownNat neg, KnownNat pos, Integral int) => GenDigit neg pos int -> Maybe Char
genDigitToChar (GenDigit ix) = gdIxToChar ix

charToGenDigit :: (KnownNat neg, KnownNat pos, Integral int) => Char -> Maybe (GenDigit neg pos int)
charToGenDigit = fmap GenDigit . charToGDIx

instance (KnownNat neg, KnownNat pos, Integral int) => Read (GenDigit neg pos int) where
  readsPrec _ [] = []
  readsPrec _ ('0' : xs) = [(0, xs)]
  readsPrec _ ((charToGDIx -> Just ix) : xs) = [(GenDigit ix, xs)]
  readsPrec d xs = [(GenDigit ix, ys) | (ix, ys) <- readsPrec d xs]

instance (KnownNat neg, KnownNat pos, Integral int) => Digit (GenDigit neg pos int) where
  fromDigit (GenDigit ix) = gdIxToIntegral ix
  toDigit int = GenDigit $ (integralToGDIx int - minGDIx) `mod` sizeGDIx + minGDIx
  digitMin = GenDigit minGDIx
  digitMax = GenDigit maxGDIx

instance (KnownNat neg, KnownNat pos, Integral int) => Num (GenDigit neg pos int) where
  GenDigit x + GenDigit y = GenDigit $ x + y
  GenDigit x * GenDigit y = GenDigit $ x * y
  GenDigit x - GenDigit y = GenDigit $ x - y
  abs (GenDigit x) = GenDigit $ abs x
  signum (GenDigit x) = GenDigit $ signum x
  fromInteger = GenDigit . fromInteger

onWithCarry :: forall neg pos int. (KnownNat neg, KnownNat pos, Integral int) => (GDIx neg pos int -> GDIx neg pos int -> GDIx neg pos int) -> GDIx neg pos int -> GDIx neg pos int -> (GDIx neg pos int, GDIx neg pos int)
onWithCarry (|?|) x y = second (minGDIx +) ((x |?| y - minGDIx) `divMod` sizeGDIx)

instance (KnownNat pos, KnownNat neg, Integral int) => Num [GenDigit pos neg int] where
  (+) = f 0
    where
      f c [] []
        | c == 0 = []
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
        | c == 0 = []
        | otherwise = [GenDigit c]
      f c [] ys = f c [GenDigit 0] ys
      f c xs [] = f c xs [GenDigit 0]
      f c (GenDigit x : xs) (GenDigit y : ys) = GenDigit z2 : f (c1 + c2) xs ys
        where
          (c1, z1) = onWithCarry (-) x y
          (c2, z2) = onWithCarry (-) z1 c
  abs = fromInteger . abs . fromDigits
  signum = fromInteger . signum . fromDigits
  fromInteger = toDigits