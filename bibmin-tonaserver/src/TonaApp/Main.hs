{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TonaApp.Main where

import Tonalude
import Tonatona (HasConfig(..), HasParser(..))
import qualified Tonatona.Logger as TonaLogger
import qualified Tonatona.Servant as TonaServer
import Servant
import Text.Megaparsec

import TonaApp.BibminAPI
import qualified TonaApp.Config as C
import Bibmin.Parse
import Bibmin.PrettyPrint


-- App


app :: RIO Config ()
app = do
  TonaLogger.logInfo $ display ("This is a bibmin-tonaserver" :: Text)
  TonaLogger.logDebug $ display ("Start server" :: Text)
  TonaServer.run @BibminAPI server


-- Implementation of server


server :: ServerT BibminAPI (RIO Config)
server = getBibmin :<|> postBibmin

getBibmin :: RIO Config MattermostResponse
getBibmin =
  return 
  $ MattermostResponse "pong!"

postBibmin :: MattermostRequest -> RIO Config MattermostResponse
postBibmin (MattermostRequest bib) = case parseBibtex' bib of
  Left err -> return 
    . MattermostResponse 
    . triquote 
    . utf8BuilderToText 
    . fromString 
    $ errorBundlePretty err
  Right bib' -> return 
    . MattermostResponse 
    . triquote 
    . textDisplay 
    . prettyPrint def' 
    $ C.transform tags bib'
  where
    tags = C.tags C.defaultConfig
    def' = C.toPPConfig $ C.format C.defaultConfig
    triquote body = "```\n" <> body <> "\n```"


-- Config


data Config = Config
  { tonaLogger :: TonaLogger.Config
  , tonaServer :: TonaServer.Config
  -- , yetAnotherPlugin :: TonaYetAnotherPlugin.Config
  }


instance HasConfig Config TonaLogger.Config where
  config = tonaLogger

instance HasConfig Config TonaServer.Config where
  config = tonaServer
  
instance HasParser Config where
  parser = Config
      <$> parser
      <*> parser
      -- <*> parser
