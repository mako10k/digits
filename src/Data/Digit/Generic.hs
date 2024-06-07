{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Digit.Generic
  ( GenDigit (),
    pattern LenientGenDigit,
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
import Data.Function (on)
import Data.List (singleton)
import GHC.TypeNats (KnownNat)

-- | Represents a generic digit type with negative bound and positive bound.
data GenDigit neg pos int where
  -- | A digit in the range from @-neg@ to @pos@. (Do not use this constructor directly, use 'GenDigit' instead.)
  StrictGenDigit :: (KnownNat neg, KnownNat pos, Integral int) => StrictGDIx neg pos int -> GenDigit neg pos int

-- | A pattern synonym for 'GenDigit'.
pattern LenientGenDigit :: (KnownNat neg, KnownNat pos, Integral int) => LGDIx neg pos int -> GenDigit neg pos int
pattern LenientGenDigit ix <- StrictGenDigit (convertGDIx -> ix)
  where
    LenientGenDigit = StrictGenDigit . convertGDIx

{-# COMPLETE LenientGenDigit #-}

pattern GenDigit :: (KnownNat neg, KnownNat pos, Integral int) => int -> GenDigit neg pos int
pattern GenDigit ix = StrictGenDigit (StrictGDIx ix)

{-# COMPLETE GenDigit #-}

-- | Check if an integer is in the range of a 'GenDigit'.
instance (KnownNat neg, KnownNat pos, Integral int) => Show (GenDigit neg pos int) where
  show (GenDigit int) = maybe (showParen True (shows (toInteger int)) "") singleton $ toCharGDIx @LGDIx @neg @pos @int (fromIntegral int)
  showList [] = showString "0"
  showList xs = showString (concatMap show (reverse xs))

instance (KnownNat neg, KnownNat pos, Integral int) => Read (GenDigit neg pos int) where
  readsPrec _ [] = []
  readsPrec _ ('0' : xs) = [(0, xs)]
  readsPrec _ ((fromCharGDIx -> Just ix) : xs) = [(StrictGenDigit ix, xs)]
  readsPrec d xs = first StrictGenDigit <$> readsPrec d xs

instance (KnownNat neg, KnownNat pos, Integral int) => Digit (GenDigit neg pos int) where
  fromDigit (GenDigit int) = fromIntegral int
  toDigit int = GenDigit $ (fromIntegral int - fromGDIx minGDIx') `mod` fromGDIx sizeGDIx' + fromGDIx minGDIx'
    where
      minGDIx' :: StrictGDIx neg pos int
      minGDIx' = minGDIx
      sizeGDIx' :: StrictGDIx neg pos int
      sizeGDIx' = sizeGDIx
  digitMin = StrictGenDigit minGDIx
  digitMax = StrictGenDigit maxGDIx

instance (KnownNat neg, KnownNat pos, Integral int) => Num (GenDigit neg pos int) where
  LenientGenDigit x + LenientGenDigit y = LenientGenDigit $ x + y
  LenientGenDigit x * LenientGenDigit y = LenientGenDigit $ x * y
  LenientGenDigit x - LenientGenDigit y = LenientGenDigit $ x - y
  abs (LenientGenDigit x) = LenientGenDigit $ abs x
  signum (LenientGenDigit x) = LenientGenDigit $ signum x
  fromInteger = LenientGenDigit . fromInteger

onWithCarry :: forall neg pos int. (KnownNat neg, KnownNat pos, Integral int) => (LGDIx neg pos int -> LGDIx neg pos int -> LGDIx neg pos int) -> LGDIx neg pos int -> LGDIx neg pos int -> (LGDIx neg pos int, LGDIx neg pos int)
onWithCarry (|?|) x y = second (minGDIx +) ((x |?| y - minGDIx) `divMod` sizeGDIx)

instance (KnownNat pos, KnownNat neg, Integral int) => Num [GenDigit pos neg int] where
  (+) = f 0
    where
      f c [] []
        | c == 0 = 0
        | otherwise = [LenientGenDigit c]
      f c [] ys = f c [0] ys
      f c xs [] = f c xs [0]
      f c (LenientGenDigit x : xs) (LenientGenDigit y : ys) = LenientGenDigit z2 : f (c1 + c2) xs ys
        where
          (c1, z1) = onWithCarry (+) x y
          (c2, z2) = onWithCarry (+) z1 c

  xs * ys = fromInteger $ on (*) fromDigits xs ys

  (-) = f 0
    where
      f c [] []
        | c == 0 = 0
        | otherwise = [LenientGenDigit c]
      f c [] ys = f c [0] ys
      f c xs [] = f c xs [0]
      f c (LenientGenDigit x : xs) (LenientGenDigit y : ys) = LenientGenDigit z2 : f (c1 + c2) xs ys
        where
          (c1, z1) = onWithCarry (-) x y
          (c2, z2) = onWithCarry (-) z1 c
  abs = fromInteger . abs . fromDigits
  signum = fromInteger . signum . fromDigits
  fromInteger = toDigits