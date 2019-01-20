module Bibmin.Bibtex where

-- | 
-- This module includes data type of BibTeX
-- see also: BibTeX Format Description <http://www.bibtex.org/Format/>

import Data.Text (Text)

-- | 
-- Bibtex data type
-- > @string { citation-key, foo = "Mrs. Foo" }
-- >  ^^^^^^   ^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^
-- >  entry        key             tags
data Bibtex = Bibtex 
  { entry :: Text -- ^ e.g. "string"
  , key   :: Text -- ^ e.g. "citation-key"
  , tags  :: [(Text, Text)] -- ^ e.g. [("foo", "Mrs. Foo")]
  } deriving (Eq, Show)

subBibtex :: [Text] -> BibTex -> BibTex
subBibtex tagKeys bibtex = bibtex { tags = subTags tagKeys }

subTags :: [Text] -> [(Text, Text)] -> [(Text, Text)]
subTags tagKeys tags = filter (`elem` tags) tagKeys