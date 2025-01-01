module SyncPlanExec (SyncResult (..), executeSyncPlan) where

import Config
import Control.Monad
import Data.Functor
import Data.Set (Set)
import Data.Set qualified as S
import Episode
import SyncPlan
import System.Directory
import System.FilePath

data SyncResult = Deleted ExistingEpisode | Copied ExistingEpisode

executeSyncPlan :: Config -> Set SyncAction -> IO [SyncResult]
executeSyncPlan config = mapM (execAction config) . S.toAscList

execAction :: Config -> SyncAction -> IO SyncResult
execAction Config{cfgSyncTargetDir} (Delete e) = do
  let file = cfgSyncTargetDir </> eeFilename e
  putStr $ "removing " <> file
  fileExists <- doesFileExist file
  if fileExists
    then removeFile file
    else putStr " (file not found!)"
  putStrLn ""

  let dir = takeDirectory file
  whenM (doesDirectoryExist dir) $ removeDirectoryIfEmpty dir

  pure $ Deleted e
execAction Config{cfgSyncTargetDir, cfgDownloadsDir} (Copy e) = do
  let targetFilename = targetFilePath e
      target = cfgSyncTargetDir </> targetFilename
  ensureDir $ takeDirectory target
  let from = cfgDownloadsDir </> epFilename e
      to = target
  putStrLn $ mconcat ["copying ", from, " → ", to]
  copyFile from to
  pure $ Copied ExistingEpisode{eeId = epId e, eeFilename = targetFilename}

ensureDir :: FilePath -> IO ()
ensureDir = createDirectoryIfMissing createParents
  where
    -- we expect to create only the podcast directory if necessary; the sync
    -- target directory should already be present
    createParents = False

removeDirectoryIfEmpty :: FilePath -> IO ()
removeDirectoryIfEmpty dir =
  whenM (listDirectory dir <&> null) $ removeDirectory dir

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mp mx = do
  p <- mp
  when p mx
