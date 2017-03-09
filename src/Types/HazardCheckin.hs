{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE TypeFamilies#-}
{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE DeriveGeneric#-}

module Types.HazardCheckin
(
  HazardCheckin(..),
  HazardType(..)
)
where

import Data.Text (Text)
import Data.Aeson
import Database.CQL.Protocol
import GHC.Generics

data HazardType = TRE | PWR | HWR | WDG | CLO | TRA | FSG deriving (Show, Eq, Generic)

data HazardCheckin = HazardCheckin
  {
    reportId :: Text
  , geohash :: Text
  , hazardType :: HazardType
  , lat :: Double
  , lon :: Double
  , created :: Integer
  , updated :: Integer
  , expires :: Integer
  , address :: Text
  , reportCount :: Integer
  , deleteCount :: Integer
  , ttl :: Maybe Integer
  } deriving (Generic, Show)

recordInstance ''HazardCheckin

instance ToJSON HazardType

instance Cql HazardType where
  ctype = Tagged TextColumn
  toCql TRE = CqlText "TRE"
  toCql PWR = CqlText "PWR"
  toCql HWR = CqlText "HWR"
  toCql WDG = CqlText "WDG"
  toCql CLO = CqlText "CLO"
  toCql TRA = CqlText "TRA"
  toCql FSG = CqlText "FSG"
  fromCql (CqlText "TRE") = Right TRE
  fromCql (CqlText "PWR") = Right PWR
  fromCql (CqlText "HWR") = Right HWR
  fromCql (CqlText "WDG") = Right WDG
  fromCql (CqlText "CLO") = Right CLO
  fromCql (CqlText "TRA") = Right TRA
  fromCql (CqlText "FSG") = Right FSG
  fromCql _ = Left "Unknown Hazard Type"

instance ToJSON HazardCheckin

