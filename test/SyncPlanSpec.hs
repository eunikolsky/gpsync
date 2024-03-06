module SyncPlanSpec (spec) where

import SyncPlan
import Test.Hspec

spec :: Spec
spec = do
  describe "getSyncPlan" $
    it "returns empty list for empty inputs" $
      getSyncPlan [] [] `shouldBe` ([] :: [Int])
