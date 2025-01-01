module Episode (Episode (..), EpisodeId, TargetFilePath, targetFilePath) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import GHC.Generics
import System.FilePath
import Text.Show.Unicode

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
  -- ^ path to file relative to gPodder's downloads dir
  }
  deriving stock (Eq, Ord, Generic)

instance Show Episode where
  show Episode{epId, epPodcastTitle, epEpisodeTitle, epFilename, epPublishedAt} =
    mconcat
      [ "Episode #"
      , show epId
      , " {podcast "
      , ushow epPodcastTitle
      , ", episode "
      , ushow epEpisodeTitle
      , ", filename "
      , ushow epFilename
      , ", published at "
      , show epPublishedAt
      , "}"
      ]

type TargetFilePath = FilePath

targetFilePath :: Episode -> TargetFilePath
targetFilePath Episode{epPodcastTitle, epEpisodeTitle, epFilename} =
  process epPodcastTitle </> process epEpisodeTitle <.> takeExtension epFilename
  where
    process = T.unpack . sanitize
    -- TODO may need to sanitize other characters too
    -- https://github.com/gpodder/gpodder/blob/master/src/gpodder/util.py#L1658
    sanitize = T.replace "/" "∕" . T.replace ":" "᠄" . T.replace "\"" "❛" . T.replace "?" "⸮"
