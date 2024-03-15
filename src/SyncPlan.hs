module SyncPlan (Episode (..), SyncAction (..), getSyncPlan, targetFilePath) where

import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import System.FilePath

type TargetFilePath = FilePath

data SyncAction = Delete !FilePath | Copy !TargetFilePath
  deriving stock (Show, Eq, Ord)

getSyncPlan :: [FilePath] -> [TargetFilePath] -> Set SyncAction
getSyncPlan newEpisodes existingEpisodes = toCopy <> toDelete
  where
    toCopy = S.map Copy $ newS S.\\ existingS
    toDelete = S.map Delete $ existingS S.\\ newS

    newS = S.fromList newEpisodes
    existingS = S.fromList existingEpisodes

data Episode = Episode
  { epPodcastTitle :: !Text
  , epEpisodeTitle :: !Text
  , epFilename :: !FilePath
  }

targetFilePath :: Episode -> TargetFilePath
targetFilePath Episode{epPodcastTitle, epEpisodeTitle} =
  T.unpack epPodcastTitle </> T.unpack epEpisodeTitle <.> "mp3"
