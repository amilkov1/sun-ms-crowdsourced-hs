{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE TypeOperators#-}
{-#LANGUAGE DataKinds#-}
{-#LANGUAGE DuplicateRecordFields#-}
{-#LANGUAGE TypeFamilies#-}

module Lib
    (
      AuthServerData
    , AuthHandler
    , Request
    , requestHeaders
    , throwError
    , err401
    , err500
    , errBody
    , mkAuthHandler
    , Context
    , roundToN
    , Handler
    , Server
    , settings
    , cassClient
    , QueryParams
    , QueryString
    , query
    , G.decode
    , T.unpack
    , runCassQ
    , Text
    ) where

--import Servant.API
import Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import Servant.Server (BasicAuthCheck (BasicAuthCheck),
                                       BasicAuthResult( Authorized
                                                      , Unauthorized
                                                      ),
                                       Context ((:.), EmptyContext),
                                       err401, err403, err500, errBody, Server,
                                       serveWithContext, Handler)
import Servant.API.BasicAuth            (BasicAuthData (BasicAuthData))
import Servant.API.Experimental.Auth    (AuthProtect)
import Servant                          (throwError)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                         mkAuthHandler)
import Network.Wai                      (Request, requestHeaders)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Map (Map, fromList)
import qualified Data.Map as M
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp         (run)
import Data.Typeable
import Data.Functor
import Data.Aeson (ToJSON)
import Data.Time.LocalTime
import Database.CQL.IO as C
import Database.CQL.Protocol (QueryParams, QueryString, Row, Cql, Tuple, TupleType, Record)
import qualified System.Logger as L
import Conf
import Control.Monad.IO.Class
import Data.Geohash as G
import Data.Text as T
import Control.Monad.Except as E
import Control.Exception (SomeException)

settings :: Settings
settings = (
   setPortNumber (fromInteger cassPort)
 . setContacts cassHost [cassHost]
 ) defSettings

cassClient :: MonadIO m => m C.ClientState
cassClient = logger >>= (\l -> C.init l settings)

logger :: MonadIO m => m L.Logger
logger = L.new L.defSettings

runCassQ :: C.Client a -> E.ExceptT SomeException IO a
runCassQ ca =  do
  g <- logger
  c <- cassClient
  E.liftIO $ runClient c $ ca

--roundToN :: (Integral a, RealFrac a) => a -> a -> Double
roundToN d n =  (fromInteger $ round $ d * (10^n)) / (10.0^^n)


