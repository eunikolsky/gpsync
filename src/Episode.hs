module Episode (Episode (..), EpisodeId, TargetFilePath, targetFilePath) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import System.FilePath

type EpisodeId = Int

{- | Regular episodes that are managed by gPodder. They are read from the
`episode + podcast` tables.
-}
data Episode = Episode
  -- it's important to sort by these two fields first
  { epPodcastTitle :: !Text
  , epPublishedAt :: !LocalTime
  , epId :: !EpisodeId
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
