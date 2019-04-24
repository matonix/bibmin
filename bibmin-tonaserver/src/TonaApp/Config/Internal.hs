{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module TonaApp.Config.Internal where

import Tonalude
import Language.Haskell.TH.Syntax
import Instances.TH.Lift ()
import qualified Data.Yaml.TH as Y
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL

import qualified Bibmin.Bibtex as Bib
import Bibmin.Bibtex (Bibtex)
import Bibmin.PrettyPrint

data Format = Format 
  { indentSize :: Int
  , isSort :: Bool
  , labelCase :: Text
  } deriving (Show, Eq, Generic, Lift)

instance Y.FromJSON Format

type Entry = Text
type TagKeys = Text
data Tags = Tags 
  { entry :: Entry
  , tagKeys :: [TagKeys]
  } deriving (Show, Eq, Generic, Lift)

instance Y.FromJSON Tags

data Config = Config
  { format :: Format
  , tags :: [Tags]
  } deriving (Show, Eq, Generic, Lift)

instance Y.FromJSON Config


-- | get subset of BibTeX following the config

transform :: [Tags] -> Bibtex -> Bibtex
transform tags bibtex =
  case lookup (T.toLower $ Bib.entry bibtex) (map toTuple tags) of
    Nothing -> bibtex
    Just tagKeys -> Bib.subBibtex tagKeys bibtex
  where
    toTuple (Tags entry tagKeys) = (T.toLower entry, tagKeys)

-- | get Bibmin.PrettyPrint.PPConfig

toPPConfig :: Format -> PPConfig
toPPConfig (Format indentSize isSort labelCase) =
  PPConfig indentSize isSort (parseCaseOrDefault $ TL.fromStrict labelCase)