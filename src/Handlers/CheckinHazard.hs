{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE TypeOperators#-}
{-#LANGUAGE DataKinds#-}
{-#LANGUAGE DuplicateRecordFields#-}
{-#LANGUAGE TypeFamilies#-}
{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE FlexibleContexts#-}
{-#LANGUAGE QuasiQuotes#-}

module Handlers.CheckinHazard
(
  uaGetHazardCheckinHandler
)
where

import Lib
import Conf
import Data.Text
import Types.Geohash (Geohash(..))
import Data.String.Interpolate
import Database.CQL.IO as Client
import Database.CQL.Protocol
import Control.Monad.Except (runExceptT, ExceptT(..))
import API
import Data.Functor.Identity
import Types.HazardCheckin (HazardCheckin)
import Control.Applicative (pure)
import Data.String as S
import Data.ByteString.Lazy (ByteString(..))
--import Control.Exception (show)

--uaGetHazardCheckinHandler :: Server UaGetHazardCheckin
uaGetHazardCheckinHandler :: Geohash -> Handler [HazardCheckin]
uaGetHazardCheckinHandler gh = ExceptT (do
  hsE <- runExceptT $ runCassQ $ readHazardsByGeohash (repr gh)
  case hsE of
    Left e -> do
      let s = read $ "Unable to retrieve hazards with" ++ (show e) :: ByteString
      return $ Left err500 {errBody = s}
    Right hs -> return $ Right hs )

readHazardsByGeohash :: MonadClient m => Text -> m [HazardCheckin]
readHazardsByGeohash gh =
   let q :: QueryString R (Identity Text) (TupleType HazardCheckin)
       q = S.fromString [i|
         SELECT TTL(fullGeohash), reportId, fullGeohash, hazardType,
         lat, lon, created, updated, address, reportCount, deleteCount
         FROM #{hazardKS}.#{geohashTable}
         WHERE geohash = ? LIMIT #{readLim};
       |]
       p = QueryParams {
         consistency = Quorum
       , skipMetaData = True
       , values = (pure gh :: Identity Text)
       , pageSize = Nothing
       , queryPagingState = Nothing
       , serialConsistency = Nothing
       }
   in fmap asRecord <$> query q p
