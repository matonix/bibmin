{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module TonaApp.BibminAPI where

import Tonalude
import Servant.API
import Data.Aeson
import GHC.Generics ()

-- | /bibmin?bibtex='@misc{ ... }'
type BibminAPI =
         "bibmin" :> Get '[JSON] Mattermost 
    :<|> "bibmin" :> ReqBody '[JSON] Text :> Post '[JSON] Mattermost

-- | Response data
data Mattermost = Mattermost
    { text :: Text
    } deriving (Generic, ToJSON)

-- | Boilerplate of servant
api :: Proxy BibminAPI
api = Proxy