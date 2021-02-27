module CodeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Code as P
import Test.QuickCheck



spec :: Spec
spec = do
    describe "reverses decoded code" $ do
        prop "positive code" prop_decoded

    describe "funcs" $ do
        it "element" $ do
            P.element 3405 2 `shouldBe` 2
        it "length" $ do
            P.len 3405 `shouldBe` 3
        it "replace" $ do
            P.replace 3405 2 3 `shouldBe` 4658
        it "sequence" $ do
            P.sequence 2 3`shouldBe` 864

    describe "search" $ do
        it "search" $ do
            P.search 4 8 28 `shouldBe` (6, 28)
            P.search 4 8 26 `shouldBe` (6, 28)
            P.search 32 64 1000 `shouldBe` (44, 1035)



prop_decoded :: Positive P.Code -> Bool
prop_decoded (Positive x) = (P.code . P.decode) x  == x

