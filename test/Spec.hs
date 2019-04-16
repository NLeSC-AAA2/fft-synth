-- ------ language="Haskell" file="test/Spec.hs"
{-# LANGUAGE DuplicateRecordFields #-}

import Test.Hspec

import Array
import Lib

main :: IO ()
main = hspec $ do
    describe "Strides.fromShape" $ do
        it "computes strides from shapes" $ do
            fromShape [3, 3, 3] 1 `shouldBe` [1, 3, 9]
            fromShape [2, 3, 5] 1 `shouldBe` [1, 2, 6]

    describe "Strides.remove" $ do
        it "drops indexed entry from list" $ do
            remove [1, 2, 3, 4] 0 `shouldBe` [2, 3, 4]
            remove [1, 2, 3, 4] 2 `shouldBe` [1, 2, 4]

    describe "Strides.replace" $ do
        it "replaces entry at index" $ do
            replace [1, 2, 3, 4] 0 7 `shouldBe` [7, 2, 3, 4]
            replace [1, 2, 3, 4] 2 7 `shouldBe` [1, 2, 7, 4]

    describe "Strides.insert" $ do
        it "inserts entry at index" $ do
            insert [1, 2, 3, 4] 0 7 `shouldBe` [7, 1, 2, 3, 4]
            insert [1, 2, 3, 4] 2 7 `shouldBe` [1, 2, 7, 3, 4]

    let a1 = floatArray "test" [4, 5]
    describe "Strides.select" $ do
        it "selects sub-array" $ do
            let a103 = select a1 0 3
            let a112 = select a1 1 2
            (shape <$> a103) `shouldBe` Right [5]
            (stride <$> a103) `shouldBe` Right [4]
            (offset <$> a103) `shouldBe` Right 3
            (shape <$> a112) `shouldBe` Right [4]
            (stride <$> a112) `shouldBe` Right [1]
            (offset <$> a112) `shouldBe` Right 8
-- ------ end
