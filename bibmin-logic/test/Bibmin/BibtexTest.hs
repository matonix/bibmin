{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}

module Bibmin.BibtexTest where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import System.FilePath
import Bibmin.Bibtex ()
import Bibmin.Parse
import Bibmin.PrettyPrint
import Data.Text.Lazy.Encoding (encodeUtf8)

test_goldenTests :: IO TestTree
test_goldenTests = do
  let dir = "test/bibfiles"
  bibFiles <- findByExtension [".bib"] dir
  return $ testGroup "bibtex golden tests"
    [ goldenVsString
        (takeBaseName bibFile)
        goldenFile
        (encodeUtf8 . prettyPrintFileDef <$> readBibtexFile bibFile)
    | bibFile <- bibFiles
    , let goldenFile = dir </> takeBaseName bibFile <.> "golden"
    ]
