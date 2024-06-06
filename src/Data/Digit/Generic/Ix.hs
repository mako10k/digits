{-# LANGUAGE DatatypeContexts #-}

module Data.Digit.Generic.Ix
  ( Ix (Ix),
    fromIx,
    toIx,
    inRangeIx,
  )
where

import Data.Data (Proxy (Proxy))
import GHC.TypeNats (KnownNat, natVal)

newtype Ix neg pos int = Ix int deriving (Eq, Ord, Num, Real, Enum, Integral)

toIx :: (Integral int, Integral int') => int -> Ix neg pos int'
toIx = Ix . fromIntegral

fromIx :: (Integral int, Integral int') => Ix neg pos int -> int'
fromIx (Ix ix) = fromIntegral ix

inRangeIx :: forall neg pos int. (KnownNat neg, KnownNat pos, Integral int) => Ix neg pos int -> Bool
inRangeIx ix = minBound <= ix && ix <= maxBound

instance (KnownNat neg, KnownNat pos, Integral int) => Bounded (Ix neg pos int) where
  minBound = Ix (-fromIntegral (natVal (Proxy @neg)))
  maxBound = Ix (fromIntegral (natVal (Proxy @pos)))

instance (KnownNat neg, KnownNat pos, Integral int) => Read (Ix neg pos int) where
  readsPrec d s = do
    (int :: Int, t) <- readsPrec d s
    let ix :: Ix neg pos int = Ix (fromIntegral int)
    True <- return $ inRangeIx ix
    return (ix, t)