module SyncPlan (SyncAction (..), getSyncPlan) where

data SyncAction = Delete FilePath | Copy FilePath
  deriving stock (Show, Eq)

getSyncPlan :: [FilePath] -> [FilePath] -> [SyncAction]
getSyncPlan newEpisodes [] = Copy <$> newEpisodes
getSyncPlan [] existingEpisodes = Delete <$> existingEpisodes
getSyncPlan _ _ = []
