-- |
-- Module      : Geohash
-- Copyright   : (c) marius a. eriksen 2009
--
-- License     : BSD3
-- Maintainer  : marius@monkey.org
-- Stability   : experimental
-- Portability : GHC
--
-- Compute geohashes as per <http://en.wikipedia.org/wiki/Geohash>.

module Data.Geohash
  ( encode
  , decode
  , decode_
  ) where

import Data.Bits
import Data.Array
import Data.Char
import Data.List

-- could pre-multiply out the various hashes (& need to distinguish
-- what they mean for even & odd values).

-- base-32 encoding using the table in the Wikipedia article, and some
-- convenient lookup tables based on it.
b32chr = "0123456789bcdefghjkmnpqrstuvwxyz"
b32 = array (0, l) $ zip [0..l] b32chr
  where l = (length b32chr) - 1
b32_ = array (48, 122) $ [(i, indexOfChr (chr i)) | i <- [48..122]]
  where indexOfChr c = maybe (-1) id (elemIndex c b32chr)
b32ToBools = array (0, l) $ map (\x -> (x, decode (b32!x))) [0..l]
  where
    l = snd $ bounds b32
    decode c = reverse . take 5 $ bits2bools $ b32_!(ord c)
    bits2bools bs =
      case bs .&. 1 of
        1 ->  True:bits2bools bs'
        0 -> False:bits2bools bs'
      where bs' = bs `shiftR` 1

bools2bits bs =
  foldl (.|.) 0 $ bits bs 0
  where bits []         _   = [0]
        bits (True:bs)  pos = bit pos:bits bs (pos + 1)
        bits (False:bs) pos =       0:bits bs (pos + 1)

b32encode bits =
  b32!index:b32encode next
  where
    (this, next) = splitAt 5 bits
    index :: Int
    index = bools2bits $ reverse this

b32decode :: String -> Maybe [Bool]
b32decode s =
  if all wellDefined ords
    then Just $ concatMap ((b32ToBools!) . (b32_!)) ords
    else Nothing
  where
    ords = map ord s
    wellDefined ix = 
      if inRange (bounds b32_) ix
        then b32_!ix /= -1
        else False

bitstring (beg, end) x
  | x > mid   =  True:bitstring (mid, end) x
  | otherwise = False:bitstring (beg, mid) x
  where
    mid = beg + ((end - beg)/2)

unbitstring (beg, end) [] = (beg, end)
unbitstring (beg, end) (x:xs)
  | x         = unbitstring (mid, end) xs
  | otherwise = unbitstring (beg, mid) xs
  where 
    mid = beg + ((end - beg)/2)

-- | Encode a geohash from the given @(lat, lon)@ pair with the given
-- precision.
-- 
-- > encode 10 (37.775, -122.419)
encode :: (Fractional a, Ord a)
       => Int                   -- ^ precision
       -> (a, a)                -- ^ @(lat, lon)@ pair
       -> Maybe String          -- ^ geohash
encode precision (lat, lon) =
  if inRange (-180, 180) lon && inRange (-90, 90) lat
    then Just $ take precision $ b32encode $ interleave lonBits latBits
    else Nothing
  where
    inRange (beg, end) x = x >= beg && x <= end
    -- only deals with infinites, so we don't care about the []-case.
    interleave (a:as) bs  = a:(interleave' as bs)
    interleave' as (b:bs) = b:(interleave  as bs)

    lonBits = bitstring (-180, 180) lon
    latBits = bitstring (-90 , 90)  lat

-- | Decode a geohash into a @(lat, lon)@ pair.
-- 
-- > decode "9q8yyk9pqd"
decode :: (Fractional a) 
       => String                -- ^ geohash
       -> Maybe (a, a)          -- ^ @(lat, lon)@
decode geohash =
  case decode_ geohash of
    Just ((latmin, latmax), (lonmin, lonmax)) ->
      Just (latmin + (latmax - latmin) / 2, lonmin + (lonmax - lonmin) / 2)
    Nothing -> Nothing

-- | Decode a geohash into ranges of latitude & longitude. This
-- reflects the precision of the decode and can be used to construct a
-- bounding box.
-- 
-- > decode_ "9q8yyk9pqd"
decode_ :: (Fractional a)
        => String                  -- ^ geohash
        -> Maybe ((a, a), (a, a))  -- ^ @((latmin, latmax), (lonmin, lonmax))@
decode_ geohash =
  case b32decode geohash of
    Just bits -> 
      let (lonBits, latBits) = deinterleave bits in
      Just $ (unbitstring (-90, 90)   latBits, 
              unbitstring (-180, 180) lonBits)
    Nothing -> 
      Nothing
  where 
    -- We can't simply unzip with a simpler deinterleave because xs
    -- and ys aren't guaranteed to be of equal length.
    deinterleave xs@(_:ys) = (deinterleave' xs, deinterleave' ys)
    deinterleave' (x:_:xs) = x:deinterleave' xs
    deinterleave' (x:_)    = [x]
    deinterleave' []       = []
