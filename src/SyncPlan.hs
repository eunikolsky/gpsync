module SyncPlan (getSyncPlan) where

getSyncPlan :: [FilePath] -> [FilePath] -> [FilePath]
getSyncPlan newEpisodes [] = newEpisodes
getSyncPlan [] existingEpisodes = existingEpisodes
getSyncPlan _ _ = []
