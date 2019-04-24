{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE TemplateHaskell #-}

module TonaApp.Config
  ( module X
  , defaultConfig
  ) where

import qualified Data.Yaml.TH as Y
import TonaApp.Config.Internal as X

defaultConfig :: Config
defaultConfig = $$(Y.decodeFile "./resources/bibmin-config.yaml")