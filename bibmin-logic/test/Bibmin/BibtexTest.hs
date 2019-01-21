{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}

module Bibmin.BibtexTest where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import System.FilePath
import Bibmin.Bibtex
import Bibmin.Parse
import Bibmin.PrettyPrint
import Data.Text.Lazy.Encoding (encodeUtf8)

test_goldenTests :: IO TestTree
test_goldenTests = do
  let dir = "test/bibfiles"
  let goldenWrap title file func = goldenVsString
        title
        (dir </> file <.> "golden")
        (encodeUtf8 . func 
          <$> readBibtexFile (dir </> file <.> "bib"))
  return $ testGroup "bibtex golden tests"
    [ goldenWrap "identity with default" "case1" 
      $ prettyPrintFile def
    , goldenWrap "identity with indent 2" "case2" 
      $ prettyPrintFile (def { indentSize = 2 })
    , goldenWrap "filtered with def" "case3" 
      $ prettyPrintFile def . map (subBibtex ["year", "author"])
    ]
