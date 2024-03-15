module SyncPlan (SyncAction (..), getSyncPlan) where

import Data.Set (Set)
import Data.Set qualified as S

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
