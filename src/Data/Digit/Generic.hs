{-# LANGUAGE PatternSynonyms #-}

module Data.Digit.Generic where

import Data.Bifunctor (Bifunctor (first))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Digit
import Data.Function (fix, on)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeNats (KnownNat, natVal)

data GenDigit n m where
  UnsafeGenDigit :: Int -> GenDigit n m

pattern GenDigit :: forall n m. (KnownNat n, KnownNat m) => Int -> GenDigit n m
pattern GenDigit i <- UnsafeGenDigit i
  where
    GenDigit i = UnsafeGenDigit $ (i - genDigitMin (Proxy @(GenDigit n m))) `mod` genDigitCnt (Proxy @(GenDigit n m)) + genDigitMin (Proxy @(GenDigit n m))

{-# COMPLETE GenDigit #-}

genDigitMin :: forall p c n m. (KnownNat n, KnownNat m) => p (c n m) -> Int
genDigitMin _ = negate $ fromIntegral $ natVal (Proxy @n)

genDigitMax :: forall p c n m. (KnownNat n, KnownNat m) => p (c n m) -> Int
genDigitMax _ = fromIntegral $ natVal (Proxy @m)

genDigitCnt :: forall p c n m. (KnownNat n, KnownNat m) => p (c n m) -> Int
genDigitCnt p = genDigitMax p - genDigitMin p + 1

instance (KnownNat n, KnownNat m) => Show (GenDigit n m) where
  show (GenDigit i)
    | 0 <= i && i < 10 = show i
    | 10 <= i && i <= 36 = [toEnum $ (i - 10) + 65]
    | -26 <= i && i < 0 = [toEnum $ 96 - i]
    | otherwise = "(" ++ show i ++ ")"
  showList [] = showString "0"
  showList xs = fix showDigits xs
    where
      showDigits _ [] = id
      showDigits rec (y : ys) = rec ys . shows y

charToDigit :: Char -> Maybe Int
charToDigit c
  | isDigit c = Just $ fromEnum c - fromEnum '0'
  | isAsciiUpper c = Just $ fromEnum c - fromEnum 'A' + 10
  | isAsciiLower c = Just $ fromEnum 'a' - fromEnum c - 1
  | otherwise = Nothing

instance (KnownNat n, KnownNat m) => Read (GenDigit n m) where
  readsPrec _ [] = [(GenDigit 0, "")]
  readsPrec _ (x : xs) = f $ charToDigit x
    where
      f (Just y) | check y = [(GenDigit y, xs)]
      f _ = map (first GenDigit) $ filter (check . fst) $ reads (x : xs)
      check y = -n <= y && y <= m
      n = genDigitMin (Proxy @(GenDigit n m))
      m = genDigitMax (Proxy @(GenDigit n m))

instance (KnownNat n, KnownNat m) => Digit (GenDigit n m) where
  fromDigit (UnsafeGenDigit i) = i
  toDigit i = UnsafeGenDigit $ (i - genDigitMin (Proxy @(GenDigit n m))) `mod` genDigitCnt (Proxy @(GenDigit n m)) + genDigitMin (Proxy @(GenDigit n m))
  digitMin = UnsafeGenDigit $ genDigitMin (Proxy @(GenDigit n m))
  digitMax = UnsafeGenDigit $ genDigitMax (Proxy @(GenDigit n m))

instance (KnownNat n, KnownNat m) => Num (GenDigit n m) where
  (+) (UnsafeGenDigit x) (UnsafeGenDigit y) = GenDigit $ x + y
  (*) (UnsafeGenDigit x) (UnsafeGenDigit y) = GenDigit $ x * y
  (-) (UnsafeGenDigit x) (UnsafeGenDigit y) = GenDigit $ x - y
  abs (UnsafeGenDigit x) = GenDigit $ abs x
  signum (UnsafeGenDigit x) = GenDigit $ signum x
  fromInteger = GenDigit . fromIntegral

carryOn :: forall n m. (KnownNat n, KnownNat m) => Proxy (GenDigit n m) -> (Int -> Int -> Int) -> Int -> Int -> (Int, Int)
carryOn p op x y = (q, r + m)
  where
    z = x `op` y
    (q, r) = (z - m) `divMod` genDigitCnt p
    m = genDigitMin p

instance (KnownNat n, KnownNat m) => Num [GenDigit n m] where
  (+) = f 0
    where
      f c [] []
        | c == 0 = []
        | otherwise = [GenDigit c]
      f c [] ys = f c [GenDigit 0] ys
      f c xs [] = f c xs [GenDigit 0]
      f c (GenDigit x : xs) (GenDigit y : ys) = GenDigit z2 : f (c1 + c2) xs ys
        where
          (c1, z1) = carryOn (Proxy @(GenDigit n m)) (+) x y
          (c2, z2) = carryOn (Proxy @(GenDigit n m)) (+) z1 c

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
          (c1, z1) = carryOn (Proxy @(GenDigit n m)) (-) x y
          (c2, z2) = carryOn (Proxy @(GenDigit n m)) (-) z1 c
  abs = fromInteger . abs . fromDigits
  signum = fromInteger . signum . fromDigits
  fromInteger = toDigits