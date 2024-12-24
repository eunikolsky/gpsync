module Main (main) where

import Config (Config (..))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeFileStrict)
import GPodderConfig (GPodderConfig (..))
import GPodderDatabase
  ( getNewEpisodes
  , withDatabase
  )
import SyncPlan
  ( ExistingEpisodeStore
  , addSyncedEpisode
  , getSyncPlan
  , getSyncedEpisodes
  , removeSyncedEpisode
  , withExistingEpisodeStore
  )
import SyncPlanExec (SyncResult (..), executeSyncPlan)
import System.Directory (getHomeDirectory)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (die, exitSuccess)
import System.FilePath ((</>))

newtype ProgArgs = ProgArgs {isDryRun :: Bool}

main :: IO ()
main = do
  args <- getProgArgs
  getConfig >>= sync args

getProgArgs :: IO ProgArgs
getProgArgs = do
  args <- getArgs
  case args of
    ["-n"] -> pure ProgArgs{isDryRun = True}
    ["-h"] -> showHelp
    [] -> pure ProgArgs{isDryRun = False}
    xs -> die $ "unrecognized arguments " <> show xs

showHelp :: IO a
showHelp = do
  name <- getProgName
  putStrLn $ mconcat [name, " [-n|-h]"]
  exitSuccess

getConfig :: IO Config
getConfig = do
  let getDefaultGPodderDir = do
        home <- getHomeDirectory
        pure $ home </> "gPodder"
  cfgGPodderDir <- lookupEnv "GPODDER_HOME" >>= maybe getDefaultGPodderDir pure
  eitherGPConfig <- eitherDecodeFileStrict @GPodderConfig $ cfgGPodderDir </> "Settings.json"
  GPodderConfig{gcSyncTargetDir} <- either (die . ("can't parse settings: " <>)) pure eitherGPConfig
  pure
    Config
      { cfgSyncTargetDir = gcSyncTargetDir
      , cfgDownloadsDir = cfgGPodderDir </> "Downloads"
      , cfgGPodderDir
      }

sync :: ProgArgs -> Config -> IO ()
sync ProgArgs{isDryRun} cfg@Config{cfgGPodderDir} = do
  episodes <- withDatabase (cfgGPodderDir </> "Database") getNewEpisodes
  withExistingEpisodeStore (cfgGPodderDir </> "gpsync.csv") $ do
    existingEpisodes <- getSyncedEpisodes
    let actions = getSyncPlan episodes existingEpisodes
    if isDryRun
      then liftIO $ mapM_ print actions
      else do
        results <- liftIO $ executeSyncPlan cfg actions
        mapM_ saveResult results

saveResult :: SyncResult -> ExistingEpisodeStore ()
saveResult (Deleted e) = removeSyncedEpisode e
saveResult (Copied e) = addSyncedEpisode e
