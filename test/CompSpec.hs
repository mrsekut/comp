module CompSpec (spec) where

import Test.Hspec
import qualified Data.Map as M
import AST.Program (Program(..), Vars, Cmd(..))
import Comp (execute)

spec :: Spec
spec = do
    describe "comp" $ do
        it "sum" $ do
            let vars = M.fromList [(1,3), (2,2)]
            let p = Program vars [(Start), (If 2 3),(Goto 6),(Dec 2),(Inc 1),(Goto 1)]
            execute p `shouldBe` 5
        it "even" $ do
            let vars = M.fromList [(1,4), (2,0)]
            let p = Program vars [(Start), (BindV 2 1),(If 2 4),(Goto 9),(Dec 2),(If 2 7),(Goto 6),(Dec 2),(Goto 2)]
            execute p `shouldBe` 4