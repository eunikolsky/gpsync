module SyncPlanSpec (spec) where

import SyncPlan
import Test.Hspec

spec :: Spec
spec = do
  describe "getSyncPlan" $ do
    it "removes all non-new episodes" $
      let synced = ["1/a.mp3", "podcast/episode.mp3", "1/b.mp3"]
          new = []
          toDelete = Delete <$> synced
      in getSyncPlan new synced `shouldBe` toDelete

    it "copies all new episodes" $
      let synced = []
          new = ["1/a.mp3", "podcast/episode.mp3", "1/b.mp3"]
          toCopy = Copy <$> new
      in getSyncPlan new synced `shouldBe` toCopy

    it "returns empty list for empty inputs" $
      getSyncPlan [] [] `shouldBe` []
