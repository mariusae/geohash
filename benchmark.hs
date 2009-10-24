module Main where

import qualified Data.Geohash as Gh
import Criterion.Main
import Text.Printf

gh precision howmany n =
  map (Gh.encode (precision+n-n)) latlons
  where 
    latlons = zip (range (-90, 90)) (range (-180, 180))
    range (x, y) = take howmany [a + b/100 | a <- [x..y], b <- [1..99]]

main = defaultMain
       [ bgroup "geohash" 
         [bench (printf "gh %d,%d" precision howmany)
                (gh precision howmany)
          | precision <- [10..12]::[Int], howmany <- [5000,17919]]
       ]
