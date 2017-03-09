{-#LANGUAGE OverloadedStrings#-}

module Types.UUID
(

)
where

import Servant.API
import Data.UUID

{-instance FromHttpApiData UUID where

  parseUrlPiece uuidS = case fromString uuidS of
    Just uuid -> Right uuid
    Nothing -> Left "Invalid UUID"
    -}
