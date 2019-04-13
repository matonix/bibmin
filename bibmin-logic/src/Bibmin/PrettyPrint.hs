{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Bibmin.PrettyPrint
  ( PPConfig (..)
  , Case(..)
  , writeBibtexFile
  , prettyPrintFile
  , prettyPrint
  , def
  ) where

import Bibmin.Bibtex
import Data.Default
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderLazy)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import qualified Data.List as L
import Web.Scotty

data PP a = PP PPConfig a

data PPConfig = PPConfig
  { indentSize :: Int
  , isSort :: Bool
  , labelCase :: Case
  } deriving (Show)

instance Default PPConfig where
  def = PPConfig def False def

data Case = None | Lower | Upper | Title deriving (Show)

instance Default Case where
  def = None

instance Parsable Case where
  parseParam t
    | t' == T.toCaseFold "none" = Right None
    | t' == T.toCaseFold "lower" = Right Lower
    | t' == T.toCaseFold "upper" = Right Upper
    | t' == T.toCaseFold "title" = Right Title
    | otherwise = Left "parseParam Case: no parse"
    where
      t' = T.toCaseFold t

instance Pretty (PP Bibtex) where
  pretty (PP 
    (PPConfig indentSize isSort labelCase) 
    (Bibtex entry key tags)) = 
      "@" <> pretty (caseF entry) <> braces content
    where
      caseF = caseModifier labelCase . T.fromStrict
      sortF = sortFunction isSort
      content = pretty (caseF key) <> comma <> line 
        <> indent indentSize prettyTags <> line
      prettyTags = vsep (punctuate comma (L.map prettyTag (sortF tags)))
      prettyTag (label, value) = pretty label 
        <+> equals <+> dquotes (pretty value)
-- 

writeBibtexFile :: PPConfig -> FilePath -> [Bibtex] -> IO ()
writeBibtexFile ppconfig path bibs =
  T.writeFile path $ prettyPrintFile ppconfig bibs

prettyPrintFile :: PPConfig -> [Bibtex] -> Text
prettyPrintFile ppconfig bibs = T.unlines 
  . L.intersperse ""
  $ L.map (prettyPrint ppconfig) bibs
     
prettyPrint :: PPConfig -> Bibtex -> Text
prettyPrint ppconfig bibtex = renderLazy 
  . layoutPretty defaultLayoutOptions 
  . pretty
  $ PP ppconfig bibtex

caseModifier :: Case -> Text -> Text
caseModifier None = id
caseModifier Lower = T.toLower
caseModifier Upper = T.toUpper
caseModifier Title = T.toTitle

sortFunction :: Ord a => Bool -> [a] -> [a]
sortFunction False = id
sortFunction True = L.sort
