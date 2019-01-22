{-# LANGUAGE OverloadedStrings #-}

module Bibmin.PrettyPrintTest where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Bibmin.PrettyPrint
import Bibmin.Bibtex

test_unitTests :: TestTree
test_unitTests = testGroup "prettyPrint" 
  [ testCase "prints bibtex with default settings" $
      prettyPrint def (Bibtex "article" "citation-key" [("foo", "Mrs. Foo")])
      @?= "@article{citation-key,\nfoo = \"Mrs. Foo\"\n}"
  , testCase "prints bibtex with indents" $
      prettyPrint def {indentSize = 2} (Bibtex "article" "citation-key" [("foo", "Mrs. Foo")])
      @?= "@article{citation-key,\n  foo = \"Mrs. Foo\"\n}"
  , testCase "prints bibtex with indents & multi tags" $
      prettyPrint def {indentSize = 2} (Bibtex "article" "citation-key" [("foo", "Mrs. Foo"), ("bar", "Mr. Bar")])
      @?= "@article{citation-key,\n  foo = \"Mrs. Foo\",\n  bar = \"Mr. Bar\"\n}"
  ]
