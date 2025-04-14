module EpisodeSpec (spec) where

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
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
      in targetFilePath episode `shouldBe` "my favorite ∕/∕root∕.mp3"

    it "sanitizes colon in titles" $
      let episode = mkEpisode "my favorite :" ":root:"
      in targetFilePath episode `shouldBe` "my favorite ᠄/᠄root᠄.mp3"

    it "sanitizes quote in titles" $
      let episode = mkEpisode "my favorite \"" "\"root\""
      in targetFilePath episode `shouldBe` "my favorite ❛/❛root❛.mp3"

    it "sanitizes question mark in titles" $
      let episode = mkEpisode "my favorite ?" "?root?"
      in targetFilePath episode `shouldBe` "my favorite ⸮/⸮root⸮.mp3"

    it "sanitizes pipe in titles" $
      let episode = mkEpisode "my favorite |" "|root|"
      in targetFilePath episode `shouldBe` "my favorite ❘/❘root❘.mp3"

    it "keeps the trailing period" $
      let episode = mkEpisode "podcast." "episode."
      in targetFilePath episode `shouldBe` "podcast./episode..mp3"

    it "truncates the filename up to 255 bytes" $
      let episode =
            mkEpisode
              "Пиратский Канадский Лось и компания"
              "2025-01-01 23᠄16᠄25 +0200 ∕ 888 - “Не ищите ошибки в логах, разговаривая с пультом и попивая кефир в своем квартале”"
      in targetFilePath episode
           `shouldBe` "Пиратский Канадский Лось и компания/2025-01-01 23᠄16᠄25 +0200 ∕ 888 - “Не ищите ошибки в логах, разговаривая с пультом и попивая кефир в своем кварт.mp3"

    forM_ [0 .. 16] $ \sub -> do
      let fnameLength = 260 - sub
      it ("truncates the filename at the utf-8 char boundary (#" <> show sub <> ")") $
        let episode = mkEpisode "a" filename
            threeByteChar = "“"
            filename = T.replicate fnameLength "b" <> threeByteChar
        in targetFilePath episode `shouldContain` "b"

mkEpisode :: Text -> Text -> Episode
mkEpisode epPodcastTitle epEpisodeTitle =
  Episode
    { epPodcastTitle
    , epEpisodeTitle
    , epId = 1
    , epFilename = "foo/123.mp3"
    , epPublishedAt = read "2024-01-01 00:00:00"
    }
