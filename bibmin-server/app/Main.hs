{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Bibmin.Bibtex
import Bibmin.Parse
import Bibmin.PrettyPrint

main :: IO ()
main =
  scotty 8080 $ do
    get "/" $ do
      text "usage: POST \\bibmin with bibtex=\"your bibtex\""
    post "/bibmin" $ do
      bibtex <- param "bibtex"
      case parseBibtex bibtex of
        Nothing -> text "parse error"
        Just bib -> text $ prettyPrint def bib