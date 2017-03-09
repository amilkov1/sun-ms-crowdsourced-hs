{-#LANGUAGE OverloadedStrings#-}

module Types.Geohash
  (
    Geohash(..)
  )
where

import Lib
import Servant.API

data Geohash = Geohash
  {
    lat :: Double
  , lon :: Double,
    repr :: Text
  } deriving (Show)

instance FromHttpApiData Geohash where

  parseUrlPiece gh = case decode $ unpack gh of
    Just (latt, long) ->
      let lat = roundToN latt 2
          lon = roundToN long 2
      in Right $ Geohash { lat = lat, lon = lon, repr = gh}
    Nothing -> Left "Invalid geohash"
