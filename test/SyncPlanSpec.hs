module SyncPlanSpec (spec) where

import Data.Set qualified as S
import Episode
import SyncPlan
import Test.Hspec

spec :: Spec
spec = do
  describe "getSyncPlan" $ do
    let ep1 = Episode 1 "1" "a" "1/a.mp3"
        ep2 = Episode 2 "podcast" "episode" "podcast/episode.mp3"
        ep5 = Episode 5 "1" "b" "1/b.mp3"

        ee1 = ExistingEpisode 1 "1/a.mp3"
        ee2 = ExistingEpisode 2 "podcast/episode.mp3"
        ee5 = ExistingEpisode 5 "1/b.mp3"

    it "removes all existing episodes" $
      let existing = [ee1, ee2, ee5]
          new = []
          toDelete = S.fromList $ Delete <$> existing
      in getSyncPlan new existing `shouldBe` toDelete

    it "removes existing episodes that are not new anymore" $
      let new = [ep5]
          existing = [ee1, ee2, ee5]
          toDelete = S.fromList $ Delete <$> [ee1, ee2]
      in getSyncPlan new existing `shouldBe` toDelete

    it "copies all new episodes" $
      let existing = []
          new = [ep2, ep1, ep5]
          toCopy = S.fromList $ Copy <$> new
      in getSyncPlan new existing `shouldBe` toCopy

    it "copies new episodes that are not existing yet" $
      let existing = [ee5]
          new = [ep1, ep2, ep5]
          toCopy = S.fromList $ Copy <$> [ep2, ep1]
      in getSyncPlan new existing `shouldBe` toCopy

    it "returns no actions for new episodes that are all existing already" $
      let existing = [ee1, ee2, ee5]
          new = [ep5, ep1, ep2]
      in getSyncPlan new existing `shouldBe` mempty

    it "ignores unknown existing episodes" $ do
      pendingWith "TODO define this use case"
    {-let new = ["1/a.mp3", "podcast/episode.mp3", "1/b.mp3"]
        existing = new <> ["1/foo.mp3", "unknown/episode.mp3"]
    getSyncPlan new existing `shouldBe` mempty-}

    it "returns no actions for empty inputs" $
      getSyncPlan [] [] `shouldBe` mempty
