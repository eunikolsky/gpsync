module SyncPlan (ExistingEpisode (..), SyncAction (..), getSyncPlan) where

import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Episode

data ExistingEpisode = ExistingEpisode
  { eeFilename :: !TargetFilePath
  , -- it would make sense to put the id first, but this way the compiler does the
    -- work by ordering the filename before the id
    eeId :: !EpisodeId
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
