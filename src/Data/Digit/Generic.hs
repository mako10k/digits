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
import Data.Bifunctor (Bifunctor (first))
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
import Data.Digit.Generic.Ix (Ix (Ix), inRangeIx, maxBoundIx, minBoundIx, toIx)
import Data.Function (fix, on)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeNats (KnownNat)

data GenDigit neg pos where
  -- | A digit in the range from @-neg@ to @pos@. (Do not use this constructor directly, use 'GenDigit' instead.)
  UnsafeGenDigit :: forall neg pos int. (Integral int) => Ix neg pos int -> GenDigit neg pos

pattern GenDigit :: forall neg pos ix. (KnownNat neg, KnownNat pos) => Ix neg pos ix -> GenDigit neg pos
pattern GenDigit ix <- (fromGenDigit -> ix)
  where
    GenDigit = toGenDigit

{-# COMPLETE GenDigit #-}

toGenDigit :: forall neg pos int. (KnownNat neg, KnownNat pos) => Ix neg pos int -> GenDigit neg pos
toGenDigit ix
  | inRangeIx ix = UnsafeGenDigit ix
  | otherwise = error "GenDigit: out of range"

fromGenDigit :: forall neg pos. (KnownNat neg, KnownNat pos) => GenDigit neg pos -> (forall int. Ix neg pos int)
fromGenDigit (UnsafeGenDigit ix) = fromIntegral ix

-- | Check if an integer is in the range of a 'GenDigit'.
instance (KnownNat neg, KnownNat pos) => Show (GenDigit neg pos) where
  show (GenDigit ix)
    | 0 <= ix && ix <= 9 = show ix
    | 10 <= ix && ix <= 36 = [toEnum $ fromEnum 'A' + fromIntegral ix - 10]
    | -26 <= ix && ix <= -1 = [toEnum $ fromEnum 'a' - fromIntegral ix - 1]
    | otherwise = "(" ++ show ix ++ ")"
  showList [] = showString "0"
  showList xs = fix go xs
    where
      go _ [] = id
      go f (y : ys) = f ys . shows y

toChar :: (KnownNat neg, KnownNat pos) => GenDigit neg pos -> Maybe Char
toChar (GenDigit ix) = toCharFromIx ix

fromChar :: (KnownNat neg, KnownNat pos) => Char -> Maybe (GenDigit neg pos)
fromChar = fmap GenDigit . fromCharToIx

toCharFromIx :: Ix neg pos int -> Maybe Char
toCharFromIx digit = foldl1 (<|>) (map ($ digit) [toDigitCharFromIx, toAsciiUpperCharFromIx, toAsciiLowerCharFromIx])

toDigitCharFromIx :: Ix neg pos int -> Maybe Char
toDigitCharFromIx ix | 0 <= ix && ix <= 9 = Just $ toEnum $ fromEnum '0' + fromIntegral ix
toDigitCharFromIx _ = Nothing

toAsciiUpperCharFromIx :: Ix neg pos int -> Maybe Char
toAsciiUpperCharFromIx ix | 10 <= ix && ix <= 36 = Just $ toEnum $ fromEnum 'A' + (fromIntegral ix - 10)
toAsciiUpperCharFromIx _ = Nothing

toAsciiLowerCharFromIx :: Ix neg pos int -> Maybe Char
toAsciiLowerCharFromIx ix | -26 <= ix && ix <= -1 = Just $ toEnum $ fromEnum 'a' - (fromIntegral ix + 1)
toAsciiLowerCharFromIx _ = Nothing

fromDigitCharToIx :: Char -> (forall int. (Integral int) => Maybe (Ix neg pos int))
fromDigitCharToIx ch
  | isDigit ch && ix <= maxBoundIx = Just ix
  | otherwise = Nothing
  where
    ix = toIx $ fromEnum ch - fromEnum '0'

fromAsciiUpperCharToIx :: Char -> (forall int. (Integral int) => Maybe (Ix neg pos int))
fromAsciiUpperCharToIx ch
  | isAsciiUpper ch && ix <= maxBoundIx = Just ix
  | otherwise = Nothing
  where
    ix = toIx $ fromEnum x - fromEnum 'A' + 10

fromAsciiLowerCharToIx :: Char -> (forall int. (Integral int) => Ix neg pos int)
fromAsciiLowerCharToIx x = toIx $ fromEnum 'a' - fromEnum x - 1

fromCharToIx :: forall neg pos int. Char -> Maybe (Ix neg pos int)
fromCharToIx c
  | isDigit c = Just $ fromIntegral $ fromEnum c - fromEnum '0'
  | isAsciiUpper c = Just $ fromIntegral $ fromEnum c - fromEnum 'A' + 10
  | isAsciiLower c = Just $ fromIntegral $ fromEnum 'a' - fromEnum c - 1
  | otherwise = Nothing

instance (KnownNat n, KnownNat m) => Read (GenDigit n m) where
  readsPrec _ [] = []
  readsPrec d (x : xs)
    | x == '0' = [(0, xs)]
    | isSpace x = readsPrec d xs
    | isDigit x && fromDigitCharToIx x <= maxBoundIx = [(GenDigit (fromDigitCharToIx x), xs)]
    | isAsciiUpper x && fromAsciiUpperCharToIx x <= maxBoundIx = [(GenDigit (fromAsciiUpperCharToIx x), xs)]
    | isAsciiLower x && fromAsciiLowerCharToIx x >= minBoundIx = [(GenDigit (fromAsciiLowerCharToIx x), xs)]
    | otherwise = []

instance (KnownNat n, KnownNat m) => Digit (GenDigit n m) where
  fromDigit (GenDigit i) = i
  toDigit i = GenDigit $ (i - n) `mod` c + n
    where
      n = minBoundIx p
      m = maxBoundIx p
      p = Proxy @(GenDigit n m)
      c = m + n + 1
  digitMin = GenDigit (n)
    where
      n = minBoundIx p
      p = Proxy @(GenDigit n m)
  digitMax = GenDigit m
    where
      m = maxBoundIx p
      p = Proxy @(GenDigit n m)

instance (KnownNat n, KnownNat m) => Num (GenDigit n m) where
  GenDigit x + GenDigit y = GenDigit $ x + y
  GenDigit x * GenDigit y = GenDigit $ x * y
  GenDigit x - GenDigit y = GenDigit $ x - y
  abs (GenDigit x) = GenDigit $ abs x
  signum (GenDigit x) = GenDigit $ signum x
  fromInteger = GenDigit . fromInteger

onWithCarry :: forall n m i. (KnownNat n, KnownNat m) => (Ix n m i -> Ix n m i -> Ix n m i) -> Ix n m i -> Ix n m i -> (Ix n m i, Ix n m i)
onWithCarry op x y = (q, r + n)
  where
    z = x `op` y
    (q, r) = (z - n) `divMod` c
    n = minBoundIx
    m = maxBoundIx
    c = m - n + 1

instance (KnownNat n, KnownNat m) => Num [GenDigit n m] where
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
          p = Proxy @(GenDigit n m)

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
          p = Proxy @(GenDigit n m)
  abs = fromInteger . abs . fromDigits
  signum = fromInteger . signum . fromDigits
  fromInteger = toDigits