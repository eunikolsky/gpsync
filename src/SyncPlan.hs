module SyncPlan (SyncAction (..), getSyncPlan) where

import Data.Set (Set)
import Data.Set qualified as S

data SyncAction = Delete FilePath | Copy FilePath
  deriving stock (Show, Eq, Ord)

getSyncPlan :: [FilePath] -> [FilePath] -> Set SyncAction
getSyncPlan newEpisodes existingEpisodes = toCopy <> toDelete
  where
    toCopy = S.map Copy $ newS S.\\ existingS
    toDelete = S.map Delete $ existingS S.\\ newS

    newS = S.fromList newEpisodes
    existingS = S.fromList existingEpisodes
