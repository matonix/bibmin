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

-- channel_id=cniah6qa73bjjjan6mzn11f4ie&
-- channel_name=town-square&
-- command=/somecommand&
-- response_url=not+supported+yet&
-- team_domain=someteam&
-- team_id=rdc9bgriktyx9p4kowh3dmgqyc&
-- text=hello+world&
-- token=xr3j5x3p4pfk7kk6ck7b4e6ghh&
-- user_id=c3a4cqe3dfy6dgopqt8ai3hydh&
-- user_name=somename

data MattermostRequest = MattermostRequest
    { text :: Text
    } deriving (Generic, Show)

instance FromForm MattermostRequest

-- Response data

data MattermostResponse = MattermostResponse
    { text :: Text
    } deriving (Generic, ToJSON)


-- Boilerplate of servant


api :: Proxy BibminAPI
api = Proxy