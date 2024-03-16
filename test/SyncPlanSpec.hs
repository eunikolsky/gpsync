module SyncPlanSpec (spec) where

import Data.List (sort, sortOn)
import Data.Set qualified as S
import Episode
import SyncPlan
import Test.Hspec

spec :: Spec
spec = do
  let ep1 =
        Episode
          { epId = 1
          , epPodcastTitle = "1"
          , epEpisodeTitle = "a"
          , epFilename = "1/a.mp3"
          , epPublishedAt = read "2023-10-09 19:23:56"
          }
      ep2 =
        Episode
          { epId = 2
          , epPodcastTitle = "podcast"
          , epEpisodeTitle = "episode"
          , epFilename = "podcast/episode.mp3"
          , epPublishedAt = read "2024-02-03 04:00:01"
          }
      ep5 =
        Episode
          { epId = 5
          , epPodcastTitle = "1"
          , epEpisodeTitle = "b"
          , epFilename = "1/b.mp3"
          , epPublishedAt = read "2022-08-08 14:35:00"
          }

      ee1 = ExistingEpisode "1/a.mp3" 1
      ee2 = ExistingEpisode "podcast/episode.mp3" 2
      ee5 = ExistingEpisode "1/b.mp3" 5

  describe "getSyncPlan" $ do
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

    it "removes episode with a different name" $
      let new = [ep5, ep2]
          ee1Name = ee1{eeFilename = "something/different.mp3"}
          existing = [ee1Name, ee2, ee5]
          toDelete = S.fromList $ Delete <$> [ee1Name]
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

    it "doesn't duplicate existing episode with a different name" $
      let ee1Name = ee1{eeFilename = "different/name.mp3"}
          existing = [ee5, ee2, ee1Name]
          new = [ep1, ep2, ep5]
      in getSyncPlan new existing `shouldBe` mempty

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

  describe "Ord SyncAction" $ do
    it "orders Delete before Copy" $
      let deletes = Delete <$> [ee5, ee1]
          copies = Copy <$> [ep5, ep1]
          isDelete (Delete _) = True
          isDelete _ = False
      in (fmap isDelete . sort) (copies <> deletes) `shouldBe` [True, True, False, False]

    it "orders `Delete`s by filename" $
      let episodes =
            [ ExistingEpisode "foo/bar.mp3" 9
            , ExistingEpisode "a/zero.mp3" 5
            , ExistingEpisode "a/one.mp3" 4
            , ExistingEpisode "zulu/alpha.mp3" 1
            ]
          expected = sortOn eeFilename episodes
      in sort (Delete <$> episodes) `shouldBe` (Delete <$> expected)

    it "orders `Copy`s by podcast title first" $
      let mkEpisode epId epPodcastTitle =
            Episode
              { epId
              , epPodcastTitle
              , epEpisodeTitle = ""
              , epFilename = ""
              , epPublishedAt = read "2024-01-01 00:00:00"
              }
          episodes =
            [ mkEpisode 1 "zero"
            , mkEpisode 5 "abc"
            , mkEpisode 9 "foo"
            ]
          expected = sortOn epPodcastTitle episodes
      in sort (Copy <$> episodes) `shouldBe` (Copy <$> expected)

    it "orders `Copy`s by publication time second" $
      let mkEpisode epId epPublishedAt =
            Episode
              { epId
              , epPublishedAt
              , epPodcastTitle = "abc"
              , epEpisodeTitle = ""
              , epFilename = ""
              }
          episodes =
            [ mkEpisode 1 $ read "2024-12-31 20:00:00"
            , mkEpisode 5 $ read "2024-01-31 10:00:00"
            , mkEpisode 9 $ read "2024-01-31 22:40:00"
            ]
          expected = sortOn epPublishedAt episodes
      in sort (Copy <$> episodes) `shouldBe` (Copy <$> expected)
