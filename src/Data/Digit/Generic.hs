{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Digit.Generic
  ( GenDigit (),
    pattern GenDigit,
    toChar,
    fromChar,
    toGenDigit,
    fromGenDigit,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace)
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
import Data.Digit.Generic.Ix (Ix, fromIx, inRangeIx, toIx)
import Data.Function (fix, on)
import GHC.TypeNats (KnownNat)

data GenDigit neg pos int where
  -- | A digit in the range from @-neg@ to @pos@. (Do not use this constructor directly, use 'GenDigit' instead.)
  UnsafeGenDigit :: Ix neg pos int -> GenDigit neg pos int

pattern GenDigit :: forall neg pos int. (KnownNat neg, KnownNat pos, Integral int) => Ix neg pos int -> GenDigit neg pos int
pattern GenDigit ix <- (fromGenDigit -> ix)
  where
    GenDigit = toGenDigit

{-# COMPLETE GenDigit #-}

toGenDigit :: forall neg pos int. (KnownNat neg, KnownNat pos, Integral int) => Ix neg pos int -> GenDigit neg pos int
toGenDigit ix
  | inRangeIx ix = UnsafeGenDigit ix
  | otherwise = error "GenDigit: out of range"

fromGenDigit :: GenDigit neg pos int -> Ix neg pos int
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

toChar :: (KnownNat neg, KnownNat pos, Integral int) => GenDigit neg pos int -> Maybe Char
toChar (GenDigit ix) = toCharFromIx ix

fromChar :: (KnownNat neg, KnownNat pos, Integral int) => Char -> Maybe (GenDigit neg pos int)
fromChar = fmap GenDigit . fromCharToIx

toCharFromIx :: (Integral int) => Ix neg pos int -> Maybe Char
toCharFromIx digit = foldl1 (<|>) (map ($ digit) [toDigitCharFromIx, toAsciiUpperCharFromIx, toAsciiLowerCharFromIx])

toDigitCharFromIx :: (Integral int) => Ix neg pos int -> Maybe Char
toDigitCharFromIx ix | 0 <= ix && ix <= 9 = Just $ toEnum $ fromEnum '0' + fromEnum ix
toDigitCharFromIx _ = Nothing

toAsciiUpperCharFromIx :: (Integral int) => Ix neg pos int -> Maybe Char
toAsciiUpperCharFromIx ix | 10 <= ix && ix <= 36 = Just $ toEnum $ fromEnum 'A' + fromEnum ix - 10
toAsciiUpperCharFromIx _ = Nothing

toAsciiLowerCharFromIx :: (Integral int) => Ix neg pos int -> Maybe Char
toAsciiLowerCharFromIx ix | -26 <= ix && ix <= -1 = Just $ toEnum $ fromEnum 'a' - fromEnum ix - 1
toAsciiLowerCharFromIx _ = Nothing

fromDigitCharToIx :: (KnownNat neg, KnownNat pos, Integral int) => Char -> Maybe (Ix neg pos int)
fromDigitCharToIx ch
  | isDigit ch && ix <= maxBound = Just ix
  | otherwise = Nothing
  where
    ix = toIx $ fromEnum ch - fromEnum '0'

fromAsciiUpperCharToIx :: (KnownNat neg, KnownNat pos, Integral int) => Char -> Maybe (Ix neg pos int)
fromAsciiUpperCharToIx ch
  | isAsciiUpper ch && ix <= maxBound = Just ix
  | otherwise = Nothing
  where
    ix = toIx $ fromEnum ch - fromEnum 'A' + 10

fromAsciiLowerCharToIx :: (KnownNat neg, KnownNat pos, Integral int) => Char -> Maybe (Ix neg pos int)
fromAsciiLowerCharToIx ch
  | isAsciiLower ch && ix > minBound = Just ix
  | otherwise = Nothing
  where
    ix = toIx $ fromEnum 'a' - fromEnum ch - 1

fromCharToIx :: forall neg pos int. (KnownNat neg, KnownNat pos, Integral int) => Char -> Maybe (Ix neg pos int)
fromCharToIx (fromDigitCharToIx -> Just ix) = Just ix
fromCharToIx (fromAsciiUpperCharToIx -> Just ix) = Just ix
fromCharToIx (fromAsciiLowerCharToIx -> Just ix) = Just ix
fromCharToIx _ = Nothing

instance (KnownNat neg, KnownNat pos, Integral int) => Read (GenDigit neg pos int) where
  readsPrec _ [] = []
  readsPrec _ ('0' : xs) = [(0, xs)]
  readsPrec d (ch : xs) | isSpace ch = readsPrec d xs
  readsPrec _ ((fromCharToIx -> Just ix) : xs) = [(GenDigit ix, xs)]
  readsPrec d xs = [(GenDigit ix, ys) | (ix, ys) <- readsPrec d xs]

instance (KnownNat neg, KnownNat pos, Integral int) => Digit (GenDigit neg pos int) where
  fromDigit (GenDigit ix) = fromIx ix
  toDigit int = GenDigit $ (toIx int - minBound) `mod` (maxBound - minBound + 1) + minBound
  digitMin = GenDigit minBound
  digitMax = GenDigit maxBound

instance (KnownNat n, KnownNat m, Integral int) => Num (GenDigit n m int) where
  GenDigit x + GenDigit y = GenDigit $ x + y
  GenDigit x * GenDigit y = GenDigit $ x * y
  GenDigit x - GenDigit y = GenDigit $ x - y
  abs (GenDigit x) = GenDigit $ abs x
  signum (GenDigit x) = GenDigit $ signum x
  fromInteger = GenDigit . fromInteger

onWithCarry :: forall n m i. (KnownNat n, KnownNat m, Integral i) => (Ix n m i -> Ix n m i -> Ix n m i) -> Ix n m i -> Ix n m i -> (Ix n m i, Ix n m i)
onWithCarry op x y = (q, r + n)
  where
    z = x `op` y
    (q, r) = (z - n) `divMod` c
    n = minBound
    m = maxBound
    c = m - n + 1

instance (KnownNat n, KnownNat m, Integral int) => Num [GenDigit n m int] where
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