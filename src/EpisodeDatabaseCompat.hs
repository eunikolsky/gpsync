{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module EpisodeDatabaseCompat () where

import Data.Int
import Data.Time
import Data.Time.Clock.POSIX
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Ok
import Episode
import SyncPlan

deriving anyclass instance FromRow Episode
deriving anyclass instance FromRow ExistingEpisode

instance FromField LocalTime where
  fromField f = case fieldData f of
    (SQLInteger i) -> Ok $ timestampToLocalTime i
    _ -> returnError ConversionFailed f "need an int"

timestampToLocalTime :: Int64 -> LocalTime
timestampToLocalTime = utcToLocalTime utc . posixSecondsToUTCTime . realToFrac
