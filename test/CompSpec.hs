module CompSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Comp
import Test.QuickCheck

spec :: Spec
spec = do
    describe "reverses decoded code" $ do
        prop "positive code" prop_decoded


prop_decoded :: Positive Code -> Bool
prop_decoded (Positive x) = (code . decode) x  == x


-- QuickCheckで書こう
-- left (pair x y) == x
-- right (pair x y) == y