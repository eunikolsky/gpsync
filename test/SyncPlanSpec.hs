module SyncPlanSpec (spec) where

import SyncPlan
import Test.Hspec

spec :: Spec
spec = do
  describe "getSyncPlan" $ do
    it "removes all existing episodes" $
      let existing = ["1/a.mp3", "podcast/episode.mp3", "1/b.mp3"]
          new = []
          toDelete = Delete <$> existing
      in getSyncPlan new existing `shouldBe` toDelete

    it "copies all new episodes" $
      let existing = []
          new = ["1/a.mp3", "podcast/episode.mp3", "1/b.mp3"]
          toCopy = Copy <$> new
      in getSyncPlan new existing `shouldBe` toCopy

    it "returns empty list for empty inputs" $
      getSyncPlan [] [] `shouldBe` []
