{-# LANGUAGE OverloadedStrings #-} 

module Bibmin.ParseSpec where

import Test.Hspec
import Bibmin.Parse
import Bibmin.Bibtex
import Text.Megaparsec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Test.Hspec.bibtexParser" $
    it "parses correct BibTeX" $
      parseMaybe bibtexParser "@article { citation-key, foo = \"Mrs. Foo\" }"
        `shouldBe` Just [Bibtex "article" "citation-key" [("foo", "Mrs. Foo")]]
