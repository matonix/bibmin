{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Default
import Web.Scotty
import Bibmin.Parse
import Bibmin.PrettyPrint

main :: IO ()
main =
  scotty 8080 $ do
    get "/" $ text "usage: POST /bibmin with bibtex=\"your bibtex\""
    post "/bibmin" $ do
      bibtex <- param "bibtex"
      ppConfig <- paramPPConfig
      case parseBibtex bibtex of
        Nothing -> text "parse error"
        Just bib -> text $ prettyPrint ppConfig bib
      where
        paramPPConfig = PPConfig 
          <$> param "indent_size" `rescue` const (return def)
          <*> param "is_sort" `rescue` const (return False)
          <*> param "label_case" `rescue` const (return def)