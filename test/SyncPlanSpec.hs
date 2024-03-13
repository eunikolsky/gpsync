module SyncPlanSpec (spec) where

import Data.Set qualified as S
import SyncPlan
import Test.Hspec

spec :: Spec
spec = do
  describe "getSyncPlan" $ do
    it "removes all existing episodes" $
      let existing = ["1/a.mp3", "podcast/episode.mp3", "1/b.mp3"]
          new = []
          toDelete = S.fromList $ Delete <$> existing
      in getSyncPlan new existing `shouldBe` toDelete

    it "removes existing episodes that are not new anymore" $
      let new = ["1/b.mp3"]
          existingOld = ["1/a.mp3", "podcast/episode.mp3"]
          existing = existingOld <> new
          toDelete = S.fromList $ Delete <$> existingOld
      in getSyncPlan new existing `shouldBe` toDelete

    it "copies all new episodes" $
      let existing = []
          new = ["1/a.mp3", "podcast/episode.mp3", "1/b.mp3"]
          toCopy = S.fromList $ Copy <$> new
      in getSyncPlan new existing `shouldBe` toCopy

    it "copies new episodes that are not existing yet" $
      let existing = ["1/b.mp3"]
          newNew = ["1/a.mp3", "podcast/episode.mp3"]
          new = newNew <> existing
          toCopy = S.fromList $ Copy <$> newNew
      in getSyncPlan new existing `shouldBe` toCopy

    it "returns no actions for empty inputs" $
      getSyncPlan [] [] `shouldBe` mempty
