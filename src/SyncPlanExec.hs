module SyncPlanExec (executeSyncPlan) where

import Config
import Data.Set (Set)
import Data.Set qualified as S
import Episode
import SyncPlan
import System.Directory
import System.FilePath

executeSyncPlan :: Config -> Set SyncAction -> IO ()
executeSyncPlan config = mapM_ (execAction config) . S.toAscList

execAction :: Config -> SyncAction -> IO ()
execAction Config{cfgSyncTargetDir} (Delete e) = do
  let file = cfgSyncTargetDir </> eeFilename e
  putStrLn $ "removing " <> file
  removeFile file
execAction Config{cfgSyncTargetDir, cfgDownloadsDir} (Copy e) = do
  let target = targetFilePath e
  ensureDir $ takeDirectory target
  let from = cfgDownloadsDir </> epFilename e
      to = cfgSyncTargetDir </> target
  putStrLn $ mconcat ["copying ", from, " → ", to]
  copyFile from to

ensureDir :: FilePath -> IO ()
ensureDir = createDirectoryIfMissing createParents
  where
    -- we expect to create only the podcast directory if necessary; the sync
    -- target directory should already be present
    createParents = False
