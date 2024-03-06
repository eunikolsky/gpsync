module LibSpec (spec) where

import Lib
import Test.Hspec

spec :: Spec
spec = do
  describe "getSyncPlan" $
    it "returns empty list for empty inputs" $
      getSyncPlan [] [] `shouldBe` ([] :: [Int])
