module SyncPlanSpec (spec) where

import SyncPlan
import Test.Hspec

spec :: Spec
spec = do
  describe "getSyncPlan" $ do
    it "removes all non-new episodes" $
      let synced = ["1/a.mp3", "podcast/episode.mp3", "1/b.mp3"]
          new = []
          toDelete = synced
      in getSyncPlan new synced `shouldBe` toDelete

    it "returns empty list for empty inputs" $
      getSyncPlan [] [] `shouldBe` []
