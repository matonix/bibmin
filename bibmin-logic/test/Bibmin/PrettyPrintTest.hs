{-# LANGUAGE OverloadedStrings #-}

module Bibmin.PrettyPrintTest where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Bibmin.PrettyPrint
import Bibmin.Bibtex
import Data.Default

test_unitTests :: TestTree
test_unitTests = testGroup "prettyPrint" 
  [ testCase "prints bibtex pretty with default settings" $
      prettyPrint def (Bibtex "article" "citation-key" [("foo", "Mrs. Foo")])
      @?= "@article{citation-key,\nfoo = \"Mrs. Foo\"\n}"
  , testCase "prints bibtex pretty with indents" $
      prettyPrint def {indentSize = 2} (Bibtex "article" "citation-key" [("foo", "Mrs. Foo")])
      @?= "@article{citation-key,\n  foo = \"Mrs. Foo\"\n}"
  ]
