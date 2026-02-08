module SidonSpec (spec) where

import Test.Hspec
import Sidon
import qualified Data.Set as S

spec :: Spec
spec = do
  describe "uniquePairs" $ do
    it "generates unique pairs from a list" $ do
      uniquePairs [1,2,3] `shouldBe` [(1,2), (1,3), (2,3)]

    it "returns empty list for single element" $ do
      uniquePairs [1] `shouldBe` []

    it "returns empty list for empty list" $ do
      uniquePairs ([] :: [Int]) `shouldBe` []

  describe "doubles" $ do
    it "creates pairs of each element with itself" $ do
      doubles [1,2,3] `shouldBe` [(1,1), (2,2), (3,3)]

    it "returns empty for empty list" $ do
      doubles ([] :: [Int]) `shouldBe` []

  describe "pairWise" $ do
    it "combines unique pairs and doubles" $ do
      pairWise [1,2] `shouldBe` [(1,2), (1,1), (2,2)]

    it "includes all combinations for three elements" $ do
      length (pairWise [1,2,3]) `shouldBe` 6  -- 3 unique pairs + 3 doubles

  describe "sidon" $ do
    it "recognizes [1,2,5,7] as a Sidon set" $ do
      sidon [1,2,5,7] `shouldBe` True

    it "recognizes [1,2,3] as NOT a Sidon set" $ do
      sidon [1,2,3] `shouldBe` False

    it "single element is a Sidon set" $ do
      sidon [1] `shouldBe` True

    it "empty list is a Sidon set" $ do
      sidon [] `shouldBe` True

    it "recognizes [0,1,4,9,15] as a Sidon set" $ do
      sidon [0,1,4,9,15] `shouldBe` True

    it "detects non-Sidon set where 1+4=2+3" $ do
      sidon [1,2,3,4] `shouldBe` False

  describe "differenceSet" $ do
    it "computes all pairwise differences including 0" $ do
      differenceSet [1,3,5] `shouldBe` S.fromList [0, -4, -2, 2, 4]

    it "always includes 0" $ do
      S.member 0 (differenceSet [1,2,3]) `shouldBe` True

    it "single element returns just {0}" $ do
      differenceSet [5] `shouldBe` S.singleton 0

  describe "disjointDifferenceSidonSet" $ do
    it "recognizes two Sidon sets with disjoint differences" $ do
      let a = [1, 2, 5]
          b = [10, 17]
      disjointDifferenceSidonSet a b `shouldBe` True

    it "rejects if first set is not Sidon" $ do
      disjointDifferenceSidonSet [1,2,3] [10,20] `shouldBe` False

    it "rejects if second set is not Sidon" $ do
      disjointDifferenceSidonSet [1,2,5] [1,2,3] `shouldBe` False

    it "rejects if difference sets overlap (beyond 0)" $ do
      let a = [1, 3]  -- differences: {0, 2, -2}
          b = [5, 7]  -- differences: {0, 2, -2}
      disjointDifferenceSidonSet a b `shouldBe` False

  describe "getUniqueRandoms" $ do
    it "generates the correct number of unique values" $ do
      result <- getUniqueRandoms 5 1 100
      length result `shouldBe` 5

    it "generates unique values" $ do
      result <- getUniqueRandoms 10 1 100
      length result `shouldBe` length (S.fromList result)

    it "generates values in range" $ do
      result <- getUniqueRandoms 5 10 20
      all (\x -> x >= 10 && x <= 20) result `shouldBe` True

  describe "maybeSidon" $ do
    it "returns Nothing or Just a valid Sidon set" $ do
      result <- maybeSidon 3 100
      case result of
        Nothing -> True `shouldBe` True
        Just s  -> sidon s `shouldBe` True
