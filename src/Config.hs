module Config (Config (..)) where

data Config = Config
  { cfgSyncTargetDir :: !FilePath
  , cfgDownloadsDir :: !FilePath
  , cfgGPodderDir :: !FilePath
  }
