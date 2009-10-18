-- |
-- Module      : Geohash
-- Copyright   : (c) marius a. eriksen 2009
--
-- License     : BSD3
-- Maintainer  : marius@monkey.org
-- Stability   : experimental
-- Portability : GHC
--
-- Compute geohashes as per @http://en.wikipedia.org/wiki/Geohash@
-- Note that the implementation pays little regard to performance at
-- this point.
-- 
-- TODO: - check ranges of input, return Maybe.
--       - return precision/error values in decode?

module Data.Geohash
  ( encode
  , decode
  , decode_
  ) where

import Data.Bits
import Data.Array
import Data.Char
import Data.List

-- base-32 encoding using the table in the Wikipedia article.
b32chr = "0123456789bcdefghjkmnpqrstuvwxyz"
b32 = array (0, length b32chr) $ zip [0..length b32chr] b32chr
b32_ = array (48, 122) $ [(i, indexOfChr (chr i)) | i <- [48..122]]
  where indexOfChr c = maybe (-1) id (elemIndex c b32chr)

bools2bits bs =
  foldl (.|.) 0 $ bits bs 0
  where bits []         _   = [0]
        bits (True:bs)  pos = bit pos:bits bs (pos + 1)
        bits (False:bs) pos =       0:bits bs (pos + 1)

bits2bools bs =
  case bs .&. 1 of
    1 ->  True:bits2bools bs'
    0 -> False:bits2bools bs'
  where bs' = bs `shiftR` 1

b32encode bits =
  b32!index:b32encode next
  where
    (this, next) = splitAt 5 bits
    index :: Int
    index = bools2bits $ reverse this

b32decode [] = []
b32decode (c:cs) =
  (reverse . take 5 $ bits2bools bits) ++ b32decode cs
  where bits = b32_!(ord c)

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

-- | Encode a geohash from the given @(lat, lon)@ pair. We return an
-- infinite list, so make sure to take only your needed precision.
-- 
-- > take 10 $ encode (37.775, -122.419)
-- "9q8yyk9pqd"
encode :: (Fractional a, Ord a) 
       => (a, a)                -- ^ @(lat, lon)@ pair
       -> String                -- ^ geohash
encode (lat, lon) =
  b32encode $ interleave lonBits latBits
  where
    -- only deals with infinites, so we don't care about the []-case.
    interleave (a:as) bs  = a:(interleave' as bs)
    interleave' as (b:bs) = b:(interleave  as bs)

    lonBits = bitstring (-180, 180) lon
    latBits = bitstring (-90 , 90)  lat

-- | Decode a geohash into a @(lat, lon)@ pair.
-- 
-- > decode "9q8yyk9pqd"
-- (37.775000631809235,-122.4189966917038)
decode :: (Fractional a) 
       => String                -- ^ geohash
       -> (a, a)                -- ^ @(lat, lon)@
decode geohash = 
  (latmin + (latmax - latmin) / 2, lonmin + (lonmax - lonmin) / 2)
  where
    ((latmin, latmax), (lonmin, lonmax)) = decode_ geohash

-- | Decode a geohash into ranges of latitude & longitude. This
-- reflects the precision of the decode and can be used to construct a
-- bounding box.
-- 
-- > decode_ "9q8yyk9pqd"
-- ((37.77499794960022,37.77500331401825),(-122.41900205612183,-122.41899132728577))
decode_ :: (Fractional a) 
        => String               -- ^ geohash
        -> ((a, a), (a, a))     -- ^ @((latmin, latmax), (lonmin, lonmax))@
decode_ geohash =
  (unbitstring (-90, 90) latBits, unbitstring (-180, 180) lonBits)
  where 
    bits = b32decode geohash
    (lonBits, latBits) = deinterleave bits

    -- We can't simply unzip with a simpler deinterleave because xs
    -- and ys aren't guaranteed to be of equal length.
    deinterleave xs@(_:ys) = (deinterleave' xs, deinterleave' ys)
    deinterleave' (x:_:xs) = x:deinterleave' xs
    deinterleave' (x:_)    = [x]
    deinterleave' []       = []
