{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}

module Bibmin.ParseTest where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
-- import Test.Tasty.Golden (goldenVsString, findByExtension)
-- import System.FilePath (takeBaseName, replaceExtension)
import Bibmin.Parse
import Bibmin.Bibtex
import Text.Megaparsec


test_unitTests :: TestTree
test_unitTests = testGroup "bibtex" 
  [ testCase "parses dquote-value bibtex" $
      parseBibtex' "@article { citation-key, foo = \"Mrs. Foo\" }"
        @?= Right (Bibtex "article" "citation-key" [("foo", "Mrs. Foo")])
  , testCase "parses brace-value bibtex" $
      parseBibtex' "@article { citation-key, foo = {Mrs. Foo} }"
        @?= Right (Bibtex "article" "citation-key" [("foo", "Mrs. Foo")])
  , testCase "parses brace-in-brace bibtex" $
      parseBibtex' "@article { citation-key, title = {{Bib}\\TeX} }"
        @?= Right (Bibtex "article" "citation-key" [("title", "{Bib}\\TeX")])
  , testCase "parses letter-value bibtex" $
      parseBibtex' "@article { citation-key, month = jan }"
        @?= Right (Bibtex "article" "citation-key" [("month", "jan")])
  , testCase "parses number-value bibtex" $
      parseBibtex' "@article { citation-key, year = 2018 }"
        @?= Right (Bibtex "article" "citation-key" [("year", "2018")])
  , testCase "parses brace-in-empty-brace-value bibtex" $
      parseBibtex' "@article { citation-key, title = {{}\\TeX} }"
        @?= Right (Bibtex "article" "citation-key" [("title", "{}\\TeX")])
  , testCase "parses empty-brace-value bibtex" $
      parseBibtex' "@article { citation-key, title = {} }"
        @?= Right (Bibtex "article" "citation-key" [("title", "")])
  , testCase "parses empty-dquote-value bibtex" $
      parseBibtex' "@article { citation-key, title = \"\" }"
        @?= Right (Bibtex "article" "citation-key" [("title", "")])
  , testCase "parses multi-tags bibtex" $
      parseBibtex' "@article { citation-key, title = {foo}, year = 2018 }"
        @?= Right (Bibtex "article" "citation-key" [("title", "foo"), ("year", "2018")])
  , testCase "parses escape-char-value bibtex" $
      parseBibtex' "@article { cage, title = \"4\\\'33\\\"\" }"
        @?= Right (Bibtex "article" "cage" [("title", "4\\\'33\\\"")])
  ]

-- goldenTests :: IO TestTree
-- goldenTests = do
--   bibtexFiles <- findByExtension [".bib"] "."
--   return $ testGroup "Bibmin.Parse.bibtex golden tests"
--     [ goldenVsString (takeBaseName) FilePath IO ByteString ]
