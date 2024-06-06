{-# LANGUAGE DatatypeContexts #-}

module Data.Digit.Generic.Ix
  ( Ix (Ix),
    fromIx,
    toIx,
    inRangeIx,
    minBoundIx,
    maxBoundIx,
  )
where

import Data.Data (Proxy (Proxy))
import GHC.TypeNats (KnownNat, natVal)

newtype Ix neg pos int = Ix int deriving (Eq, Ord, Num, Real, Enum, Integral)

toIx :: (Integral int, Integral int') => int -> Ix neg pos int'
toIx = Ix . fromIntegral

fromIx :: (Integral int, Integral int') => Ix neg pos int -> int'
fromIx (Ix ix) = fromIntegral ix

inRangeIx :: (KnownNat neg, KnownNat pos, Integral int) => Ix neg pos int -> Bool
inRangeIx ix = minBoundIx <= ix && ix <= maxBoundIx

minBoundIx :: forall neg pos int. (KnownNat neg, Integral int) => Ix neg pos int
minBoundIx = -toIx (natVal (Proxy @neg))

maxBoundIx :: forall neg pos int. (KnownNat pos, Integral int) => Ix neg pos int
maxBoundIx = toIx (natVal (Proxy @pos))
