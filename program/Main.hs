module Main (main) where

import Config (Config (..))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeFileStrict)
import GPodderConfig (GPodderConfig (..))
import GPodderDatabase
  ( DB
  , addSyncedEpisode
  , getNewEpisodes
  , getSyncedEpisodes
  , removeSyncedEpisode
  , withDatabase
  )
import SyncPlan (getSyncPlan)
import SyncPlanExec (SyncResult (..), executeSyncPlan)
import System.Directory (getHomeDirectory)
import System.Exit (die)
import System.FilePath ((</>))

main :: IO ()
main = getConfig >>= sync

getConfig :: IO Config
getConfig = do
  home <- getHomeDirectory
  let cfgGPodderDir = home </> "gPodder"
  eitherGPConfig <- eitherDecodeFileStrict @GPodderConfig $ cfgGPodderDir </> "Settings.json"
  GPodderConfig{gcSyncTargetDir} <- either (die . ("can't parse settings: " <>)) pure eitherGPConfig
  pure
    Config
      { cfgSyncTargetDir = gcSyncTargetDir
      , cfgDownloadsDir = cfgGPodderDir </> "Downloads"
      , cfgGPodderDir
      }

sync :: Config -> IO ()
sync cfg@Config{cfgGPodderDir} = withDatabase (cfgGPodderDir </> "Database") $ do
  episodes <- getNewEpisodes
  existingEpisodes <- getSyncedEpisodes
  let actions = getSyncPlan episodes existingEpisodes
  results <- liftIO $ executeSyncPlan cfg actions
  mapM_ saveResult results

saveResult :: SyncResult -> DB ()
saveResult (Deleted e) = removeSyncedEpisode e
saveResult (Copied e) = addSyncedEpisode e
