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
  where
    getBibmin :: RIO Config MattermostResponse
    getBibmin = return $ MattermostResponse "post your bibtex content!"

    postBibmin :: MattermostRequest -> RIO Config MattermostResponse
    postBibmin (MattermostRequest bib) = return $ MattermostResponse $ case parseBibtex' bib of
      Left err -> utf8BuilderToText $ fromString $ errorBundlePretty err
      Right bib' -> textDisplay $ prettyPrint def' bib'
      where
        def' = def { indentSize = 2 }


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
