module SyncPlan (SyncAction (..), getSyncPlan) where

import Data.List ((\\))

data SyncAction = Delete FilePath | Copy FilePath
  deriving stock (Show, Eq)

getSyncPlan :: [FilePath] -> [FilePath] -> [SyncAction]
getSyncPlan newEpisodes [] = Copy <$> newEpisodes
getSyncPlan newEpisodes existingEpisodes = Delete <$> existingEpisodes \\ newEpisodes
