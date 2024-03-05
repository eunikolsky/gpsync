module LibSpec (spec) where

import Lib
import Test.Hspec

spec :: Spec
spec = do
  describe "add" $
    it "adds two numbers" $
      2 `add` 2 `shouldBe` 4
