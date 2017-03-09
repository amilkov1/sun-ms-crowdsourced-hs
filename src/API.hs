{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE TypeOperators#-}
{-#LANGUAGE DataKinds#-}
{-#LANGUAGE TypeFamilies#-}

module API
(
  UaGetHazardCheckin,
  --UaGetHazardCheckinFilter,
  --UaPostHazardCheckin,
  --UaPutHazardCheckin,
  --UnAuthenticatedHazardAPI,
  --GetHazardCheckin,
  --PostHazardCheckin,
  --PutHazardCheckin,
  --DeleteHazardCheckin,
  --AuthenticatedHazardAPI,
  --HazardAPI
)
where

import Servant.API
import Types.Geohash
import Lib
import Data.Aeson
import Data.Aeson.Types
import Data.UUID
import Types.HazardCheckin (HazardCheckin)

type UaGetHazardCheckin = "hazard" :> Capture "geohash" Geohash :> Get '[JSON] [HazardCheckin]
--type UaGetHazardCheckinFilter =  UaGetHazardCheckin :<|> Capture "filter" Int
--type UaPostHazardCheckin = "checkin/hazard" :> ReqBody '[JSON] Hazard :> Post '[JSON] CrowdsourcedResp
--type UaPutHazardCheckin = "checkin/hazard" :> Capture "geohash" Geohash :> Capture "reportId" UUID :> Put '[JSON] CrowdsourcedResp

--type UnAuthenticatedHazardAPI = UaGetHazardCheckin :> UaPostHazardCheckin :> UaPutHazardCheckin

--type GetHazardCheckin = "p/checkin/hazard" :> AuthProtect "dsx" :> Get '[JSON] [UserHazardCheckin]
--type PostHazardCheckin = "p/checkin/hazard" :> AuthProtect "dsx" :> ReqBody '[JSON] Hazard :> Post '[JSON] CrowdsourcedResp
--type PutHazardCheckin = "p/checkin/hazard" :> AuthProtect "dsx" :> Capture "geohash" Geohash :> Capture "reportId" UUID :> Put '[JSON] CrowdsourcedResp
--type DeleteHazardCheckin = "p/checkin/hazard" :> AuthProtect "dsx" :> Capture "geohash" Geohash :> Capture "reportId" UUID :> Delete '[JSON] CrowdsourcedResp

--type AuthenticatedHazardAPI = GetHazardCheckin :<|> PostHazardCheckin :<|> PutHazardCheckin :<|> DeleteHazardCheckin
--type AuthenticatedHazardAPI = UaGetHazardCheckin

--type HazardAPI = AuthenticatedHazardAPI :<|> UnAuthenticatedHazardAPI
