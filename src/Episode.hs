module Episode (Episode (..), EpisodeId, TargetFilePath, targetFilePath) where

import Data.Text (Text)
import Data.Text qualified as T
import System.FilePath

type EpisodeId = Int

data Episode = Episode
  { epId :: !EpisodeId
  , epPodcastTitle :: !Text
  , epEpisodeTitle :: !Text
  , epFilename :: !FilePath
  }
  deriving stock (Show, Eq, Ord)

type TargetFilePath = FilePath

targetFilePath :: Episode -> TargetFilePath
targetFilePath Episode{epPodcastTitle, epEpisodeTitle} =
  process epPodcastTitle </> process epEpisodeTitle <.> "mp3"
  where
    process = T.unpack . sanitize
    -- TODO may need to sanitize other characters too
    -- https://github.com/gpodder/gpodder/blob/master/src/gpodder/util.py#L1658
    sanitize = T.replace "/" "_"
