module SyncPlan (SyncAction (..), getSyncPlan) where

import Data.List ((\\))

data SyncAction = Delete FilePath | Copy FilePath
  deriving stock (Show, Eq)

getSyncPlan :: [FilePath] -> [FilePath] -> [SyncAction]
getSyncPlan newEpisodes existingEpisodes = toCopy <> toDelete
  where
    toCopy = Copy <$> newEpisodes \\ existingEpisodes
    toDelete = Delete <$> existingEpisodes \\ newEpisodes
