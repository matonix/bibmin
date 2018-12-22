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

