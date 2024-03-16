module EpisodeSpec (spec) where

import Data.Text (Text)
import Episode
import Test.Hspec

spec :: Spec
spec = do
  describe "targetFilePath" $ do
    it "consists of podcast and episode titles" $
      let episode = mkEpisode "foo" "bar"
      in targetFilePath episode `shouldBe` "foo/bar.mp3"

    it "sanitizes slash in titles" $
      let episode = mkEpisode "my favorite /" "/root/"
      in targetFilePath episode `shouldBe` "my favorite _/_root_.mp3"

    it "keeps the trailing period" $
      let episode = mkEpisode "podcast." "episode."
      in targetFilePath episode `shouldBe` "podcast./episode..mp3"

mkEpisode :: Text -> Text -> Episode
mkEpisode epPodcastTitle epEpisodeTitle =
  Episode
    { epPodcastTitle
    , epEpisodeTitle
    , epId = 1
    , epFilename = "foo/123.mp3"
    }
