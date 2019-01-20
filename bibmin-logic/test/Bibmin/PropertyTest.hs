module Bibmin.PropertyTest where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Bibmin.PrettyPrint
import Bibmin.Bibtex
import Bibmin.Parse
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as Lazy

test_property :: TestTree
test_property = testGroup "tasty-hedgehog tests"
  [ testProperty
      "parse is left inverse of pretty print" $
      property $ do
        x <- forAll genBibtex
        leftInverse parseBibtex prettyPrintDef x
        
  ]
  where
    genBibtex :: Gen Bibtex
    genBibtex = Bibtex <$> genTextNonEmpty <*> genTextNonEmpty <*> genTags
      where
        genTags = Gen.list (Range.linear 1 10) genPair
        genPair = (,) <$> genTextNonEmpty <*> genText
        genText = pack <$> Gen.list (Range.linear 0 100) Gen.alpha
        genTextNonEmpty = pack <$> Gen.list (Range.linear 1 100) Gen.alpha
    
    leftInverse :: MonadTest m => 
      (Text -> Maybe Bibtex) -> 
      (Bibtex -> Lazy.Text) -> 
      Bibtex -> m ()
    leftInverse g f x =
      g (Lazy.toStrict (f x)) === Just x
    