module GPodderConfig (GPodderConfig (..)) where

import Data.Aeson

newtype GPodderConfig = GPodderConfig {gcSyncTargetDir :: FilePath}
  deriving stock (Show, Eq)

instance FromJSON GPodderConfig where
  parseJSON = withObject "GPodderConfig" $ \o -> do
    deviceSync <- o .: "device_sync"
    gcSyncTargetDir <- deviceSync .: "device_folder"
    pure $ GPodderConfig{gcSyncTargetDir}
