{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module TonaApp.BibminAPI where

import Tonalude
import Servant.API
import Data.Aeson
import GHC.Generics ()
import Web.FormUrlEncoded


-- /bibmin?bibtex='@misc{ ... }'


type BibminAPI =
         "bibmin" :> Get '[JSON] MattermostResponse 
    :<|> "bibmin" :> ReqBody '[FormUrlEncoded] MattermostRequest :> Post '[JSON] MattermostResponse


-- Request data


data MattermostRequest = MattermostRequest
    { text :: Text 
    } deriving (Generic)

instance FromForm MattermostRequest


-- Response data


data MattermostResponse = MattermostResponse
    { text :: Text
    } deriving (Generic, ToJSON)


-- Boilerplate of servant


api :: Proxy BibminAPI
api = Proxy