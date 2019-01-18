{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Bibmin.PrettyPrint where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderLazy)
import Data.Text.Lazy
import Bibmin.Bibtex
import Data.Default
import qualified Data.List as L

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

instance Pretty (PP Bibtex) where
  pretty (PP 
    (PPConfig indentSize isSort labelCase) 
    (Bibtex entry key tags)) = 
      "@" <> pretty (caseF entry) <> braces content
    where
      caseF = caseModifier labelCase . fromStrict
      sortF = sortFunction isSort
      content = pretty (caseF key) <> comma <> line 
        <> indent indentSize (prettyTags) <> line
      prettyTags = vsep (punctuate comma (L.map prettyTag (sortF tags)))
      prettyTag (label, value) = pretty label 
        <+> equals <+> dquotes (pretty value)


caseModifier :: Case -> Text -> Text
caseModifier None = id
caseModifier Lower = toLower
caseModifier Upper = toUpper
caseModifier Title = toTitle

sortFunction :: Ord a => Bool -> [a] -> [a]
sortFunction False = id
sortFunction True = L.sort

prettyPrint :: PPConfig -> Bibtex -> Text
prettyPrint ppconfig bibtex = renderLazy 
  . layoutPretty defaultLayoutOptions 
  . pretty
  $ PP ppconfig bibtex