{-# LANGUAGE UndecidableInstances #-}
module Data.Digit where
import Data.Function (on)

class Digit d where
  {-# MINIMAL
    (toDigit | toDigitGeneric),
    (fromDigit | fromDigitGeneric),
    (digitMin | digitMinValue | digitMinValueGeneric),
    (digitMax | digitMaxValue | digitMaxValueGeneric)
    #-}

  toDigit :: Int -> d
  toDigit = toDigitGeneric

  toDigitGeneric :: (Integral i) => i -> d
  toDigitGeneric = toDigit . fromIntegral

  digitMin :: d
  digitMin = toDigit (fromDigit (digitMin :: d))

  digitMax :: d
  digitMax = toDigit (fromDigit (digitMax :: d))

  fromDigit :: d -> Int
  fromDigit = fromDigitGeneric

  fromDigitGeneric :: (Integral i) => d -> i
  fromDigitGeneric = fromIntegral . fromDigit

  digitMinValue :: proxy d -> Int
  digitMinValue _ = fromDigit (digitMin :: d)

  digitMaxValue :: proxy d -> Int
  digitMaxValue _ = fromDigit (digitMax :: d)

  digitCount :: proxy d -> Int
  digitCount p = digitMaxValue p - digitMinValue p + 1

  digitMinValueGeneric :: (Integral i) => proxy d -> i
  digitMinValueGeneric _ = fromDigitGeneric (digitMin :: d)

  digitMaxValueGeneric :: (Integral i) => proxy d -> i
  digitMaxValueGeneric _ = fromDigitGeneric (digitMax :: d)

  digitCountGeneric :: (Integral i) => proxy d -> i
  digitCountGeneric p = digitMaxValueGeneric p - digitMinValueGeneric p + 1

  toDigits :: (Integral i) => i -> [d]
  toDigits 0 = []
  toDigits n = toDigit (r + m) : toDigits q
    where
      n' = fromIntegral n
      m = fromDigit (digitMin :: d)
      (q, r) = (n' - m) `divMod` (fromDigit (digitMax :: d) - m + 1)

  fromDigits :: (Integral i) => [d] -> i
  fromDigits = foldr (\d acc -> acc * (maxDigitIntegral - minDigitIntegral + 1) + fromDigitGeneric d) 0
    where
      minDigitIntegral = fromDigitGeneric (digitMin :: d)
      maxDigitIntegral = fromDigitGeneric (digitMax :: d)

instance Digit Bool where
  toDigit 0 = False
  toDigit 1 = True
  toDigit n = toDigit (n `mod` 2)
  fromDigit False = 0
  fromDigit True = 1
  digitMin = False
  digitMax = True

data Trit = Neg | Zero | Pos
  deriving (Eq, Ord, Show)

instance Digit Trit where
  toDigit (-1) = Neg
  toDigit 0 = Zero
  toDigit 1 = Pos
  toDigit n = toDigit (((n + 1) `mod` 3) - 1)
  fromDigit Neg = -1
  fromDigit Zero = 0
  fromDigit Pos = 1
  digitMin = Neg
  digitMax = Pos

trimDigits :: (Digit d) => [d] -> [d]
trimDigits [] = []
trimDigits (x : xs)
  | fromDigit x == 0 && null rs = []
  | otherwise = x : rs
  where
    rs = trimDigits xs

