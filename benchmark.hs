module Main where

import qualified Data.Geohash as Gh
import Criterion.Main
import Text.Printf
import Data.Array

hashes = array (0, 2) [(0, "9q8yyk9pqd"), (1, "9q8yyk9p"), (2, "9q8yyk")]

-- geohash encoder helper
ghe precision howmany n =
  map (Gh.encode (precision+n-n)) latlons
  where 
    latlons = zip (range (-90, 90)) (range (-180, 180))
    range (x, y) = take howmany [a + b/100 | a <- [x..y], b <- [1..99]]

main = defaultMain
       [ bgroup "encode"
         [bench (printf "%d,%d" precision howmany) (ghe precision howmany)
          | precision <- [10..12]::[Int], howmany <- [5000,17919]]

       , bgroup "decode"
         [ bench "0" (\n -> Gh.decode (hashes!(0+n-n::Int)))
         , bench "1" (\n -> Gh.decode (hashes!(1+n-n::Int)))
         , bench "2" (\n -> Gh.decode (hashes!(2+n-n::Int)))
         ]
       ]
