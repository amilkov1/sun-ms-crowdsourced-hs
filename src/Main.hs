{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE TypeOperators#-}
{-#LANGUAGE DataKinds#-}
{-#LANGUAGE DuplicateRecordFields#-}
{-#LANGUAGE TypeFamilies#-}
{-#LANGUAGE DeriveGeneric#-}

module Main where

import Lib
import API
import Handlers.CheckinHazard (uaGetHazardCheckinHandler)
--import Lib(CrowdsourcedResp(CrowdsourcedResp(..)))
import Data.Time.LocalTime
import qualified Data.Map as M
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Proxy (Proxy (Proxy))
import Servant.Server (BasicAuthCheck (BasicAuthCheck),
                                       BasicAuthResult( Authorized
                                                      , Unauthorized
                                                      ),
                                       Context ((:.), EmptyContext),
                                       err401, err403, errBody, Server,
                                       serveWithContext, Handler)

import Servant.API
import Servant (serve)
import GHC.Generics
import Data.Aeson
import Types.Geohash
import Types.HazardCheckin

--auth middleware
{-type instance AuthServerData (AuthProtect "dsx") = CrowdsourcedResp

genAuthAPI :: Proxy AuthenticatedHazardAPI
genAuthAPI = Proxy

genAuthServerContext :: Context (AuthHandler Request CrowdsourcedResp ': '[])
genAuthServerContext = authHandler :. EmptyContext


authHandler :: AuthHandler Request CrowdsourcedResp
authHandler =
  let handler req = case lookup "dsx" (requestHeaders req) of
        Nothing -> throwError (err401 {errBody = "Missing dsx cookie"})
        Just dsxCookie -> CrowdsourcedResp {code = 200, body = "Dope"}
  in mkAuthHandler handler -}

type AnAPI = UaGetHazardCheckin

api :: Proxy AnAPI
api = Proxy

server :: Server AnAPI
server = uaGetHazardCheckin

  where uaGetHazardCheckin :: Geohash -> Handler [HazardCheckin]
        uaGetHazardCheckin = uaGetHazardCheckinHandler

app :: Application
app = serve api server

main :: IO ()
main = do
  print "Running app"
  run 8080 app
