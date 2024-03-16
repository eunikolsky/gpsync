module EpisodeSpec (spec) where

import Episode
import Test.Hspec

spec :: Spec
spec = do
  describe "targetFilePath" $ do
    it "consists of podcast and episode titles" $
      let episode = Episode 1 "foo" "bar" "foo/123.mp3"
      in targetFilePath episode `shouldBe` "foo/bar.mp3"

    it "sanitizes slash in titles" $
      let episode = Episode 1 "my favorite /" "/root/" "foo/123.mp3"
      in targetFilePath episode `shouldBe` "my favorite _/_root_.mp3"

    it "keeps the trailing period" $
      let episode = Episode 1 "podcast." "episode." "foo/123.mp3"
      in targetFilePath episode `shouldBe` "podcast./episode..mp3"
