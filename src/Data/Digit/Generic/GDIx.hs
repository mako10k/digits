{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Digit.Generic.GDIx
  ( GDIx (GDIx),
    gdIxToIntegral,
    integralToGDIx,
    isProperGDIx,
    minGDIx,
    maxGDIx,
    sizeGDIx,
    gdIxToChar,
    charToGDIx,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Data (Proxy (Proxy))
import GHC.TypeNats (KnownNat, natVal)

newtype GDIx neg pos int = GDIx int deriving (Eq, Ord, Num, Real, Enum, Integral)

integralToGDIx :: (Integral int, Integral int') => int -> GDIx neg pos int'
integralToGDIx = GDIx . fromIntegral

gdIxToIntegral :: (Integral int, Integral int') => GDIx neg pos int -> int'
gdIxToIntegral (GDIx ix) = fromIntegral ix

isProperGDIx :: forall neg pos int. (KnownNat neg, KnownNat pos, Integral int) => GDIx neg pos int -> Bool
isProperGDIx ix = minGDIx <= ix && ix <= maxGDIx

minGDIx :: forall neg pos int. (KnownNat neg, Integral int) => GDIx neg pos int
minGDIx = GDIx (-fromIntegral (natVal (Proxy @neg)))

maxGDIx :: forall neg pos int. (KnownNat pos, Integral int) => GDIx neg pos int
maxGDIx = GDIx (fromIntegral (natVal (Proxy @pos)))

sizeGDIx :: forall neg pos int. (KnownNat neg, KnownNat pos, Integral int) => GDIx neg pos int
sizeGDIx = maxGDIx - minGDIx + 1

instance (KnownNat neg, KnownNat pos, Integral int) => Read (GDIx neg pos int) where
  readsPrec d s = do
    (int :: Int, t) <- readsPrec d s
    let ix :: GDIx neg pos int = GDIx (fromIntegral int)
    True <- return $ isProperGDIx ix
    return (ix, t)

gdIxToChar :: (Integral int) => GDIx neg pos int -> Maybe Char
gdIxToChar digit = foldl1 (<|>) (map ($ digit) [gdIxToCharDigit, gdIxToCharAsciiUpper, gdIxToCharAsciiLower])

gdIxToCharDigit :: (Integral int) => GDIx neg pos int -> Maybe Char
gdIxToCharDigit ix | 0 <= ix && ix <= 9 = Just $ toEnum $ fromEnum '0' + fromEnum ix
gdIxToCharDigit _ = Nothing

gdIxToCharAsciiUpper :: (Integral int) => GDIx neg pos int -> Maybe Char
gdIxToCharAsciiUpper ix | 10 <= ix && ix <= 36 = Just $ toEnum $ fromEnum 'A' + fromEnum ix - 10
gdIxToCharAsciiUpper _ = Nothing

gdIxToCharAsciiLower :: (Integral int) => GDIx neg pos int -> Maybe Char
gdIxToCharAsciiLower ix | -26 <= ix && ix <= -1 = Just $ toEnum $ fromEnum 'a' - fromEnum ix - 1
gdIxToCharAsciiLower _ = Nothing

charDigitToGDIx :: (KnownNat neg, KnownNat pos, Integral int) => Char -> Maybe (GDIx neg pos int)
charDigitToGDIx ch
  | isDigit ch && ix <= maxGDIx = Just ix
  | otherwise = Nothing
  where
    ix = integralToGDIx $ fromEnum ch - fromEnum '0'

charAsciiUpperToGDIx :: (KnownNat neg, KnownNat pos, Integral int) => Char -> Maybe (GDIx neg pos int)
charAsciiUpperToGDIx ch
  | isAsciiUpper ch && ix <= maxGDIx = Just ix
  | otherwise = Nothing
  where
    ix = integralToGDIx $ fromEnum ch - fromEnum 'A' + 10

charAsciiLowerToGDIx :: (KnownNat neg, KnownNat pos, Integral int) => Char -> Maybe (GDIx neg pos int)
charAsciiLowerToGDIx ch
  | isAsciiLower ch && ix > minGDIx = Just ix
  | otherwise = Nothing
  where
    ix = integralToGDIx $ fromEnum 'a' - fromEnum ch - 1

charToGDIx :: forall neg pos int. (KnownNat neg, KnownNat pos, Integral int) => Char -> Maybe (GDIx neg pos int)
charToGDIx (charDigitToGDIx -> Just ix) = Just ix
charToGDIx (charAsciiUpperToGDIx -> Just ix) = Just ix
charToGDIx (charAsciiLowerToGDIx -> Just ix) = Just ix
charToGDIx _ = Nothing
