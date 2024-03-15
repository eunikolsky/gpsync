module SyncPlan (Episode (..), ExistingEpisode (..), SyncAction (..), getSyncPlan, targetFilePath) where

import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import System.FilePath

type TargetFilePath = FilePath

data ExistingEpisode = ExistingEpisode
  { eeId :: !EpisodeId
  , eeFilename :: !TargetFilePath
  }
  deriving stock (Show, Eq, Ord)

data SyncAction = Delete !ExistingEpisode | Copy !Episode
  deriving stock (Show, Eq, Ord)

getSyncPlan :: [Episode] -> [ExistingEpisode] -> Set SyncAction
getSyncPlan newEpisodes existingEpisodes = toCopy <> toDelete
  where
    toCopy = S.fromList . fmap Copy . M.elems $ newM M.\\ existingM
    toDelete = S.fromList . fmap Delete . M.elems $ existingM M.\\ newM

    newM = M.fromList $ (\e -> (epId e, e)) <$> newEpisodes
    existingM = M.fromList $ (\e -> (eeId e, e)) <$> existingEpisodes

type EpisodeId = Int

data Episode = Episode
  { epId :: !EpisodeId
  , epPodcastTitle :: !Text
  , epEpisodeTitle :: !Text
  , epFilename :: !FilePath
  }
  deriving stock (Show, Eq, Ord)

targetFilePath :: Episode -> TargetFilePath
targetFilePath Episode{epPodcastTitle, epEpisodeTitle} =
  process epPodcastTitle </> process epEpisodeTitle <.> "mp3"
  where
    process = T.unpack . sanitize
    -- TODO may need to sanitize other characters too
    -- https://github.com/gpodder/gpodder/blob/master/src/gpodder/util.py#L1658
    sanitize = T.replace "/" "_"
