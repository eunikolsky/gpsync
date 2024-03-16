module SyncPlan (ExistingEpisode (..), SyncAction (..), getSyncPlan) where

import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Episode

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
