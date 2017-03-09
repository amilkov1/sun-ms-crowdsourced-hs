module Conf
(
  cassHost
, cassPort
, hazardKS
, geohashTable
, readLim
)
where

import Data.Int (Int32)

cassHost = "localhost"
cassPort = 9042
hazardKS = "hazards"
geohashTable = "hazardsbygeohash"
readLim = 200 :: Int32
