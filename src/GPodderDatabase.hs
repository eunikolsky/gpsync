{-# LANGUAGE QuasiQuotes #-}

module GPodderDatabase
  ( DB
  , addSyncedEpisode
  , getNewEpisodes
  , getSyncedEpisodes
  , removeSyncedEpisode
  , withDatabase
  ) where

import Control.Monad.Reader
import Database.SQLite.Simple
import Episode
import EpisodeDatabaseCompat ()
import SyncPlan
import Text.RawString.QQ

-- | Internal, opaque type to wrap the database `Connection`.
type DB a = ReaderT Connection IO a

{- | Wraps `withConnection` in order not to require other modules to import
`Database.SQLite.Simple`.
-}
withDatabase :: FilePath -> DB a -> IO a
withDatabase file f = withConnection file . runReaderT $ createSyncedEpisodeTable >> f

{- | Returns all not-listened-to, downloaded episodes from the gPodder database.
Only @.mp3@ files are returned. The filenames are relative to gPodder's download
directory (they look like `podcast/episode.mp3`).
-}
getNewEpisodes :: DB [Episode]
getNewEpisodes = do
  conn <- ask
  liftIO $
    query_
      conn
      [r|
      SELECT p.title
        , e.published
        , e.id
        , e.title
        , p.download_folder || '/' || e.download_filename
      FROM episode e
      JOIN podcast p ON e.podcast_id = p.id
      WHERE e.state = 1
        AND e.is_new
        AND e.download_filename LIKE '%.mp3'
    |]

addSyncedEpisode :: ExistingEpisode -> DB ()
addSyncedEpisode ExistingEpisode{eeId, eeFilename} = do
  conn <- ask
  liftIO $
    executeNamed
      conn
      "INSERT INTO synced_episode (episodeId, filename) VALUES (:id, :filename)"
      [":id" := eeId, ":filename" := eeFilename]

removeSyncedEpisode :: ExistingEpisode -> DB ()
removeSyncedEpisode ExistingEpisode{eeId} = do
  conn <- ask
  liftIO $
    executeNamed
      conn
      "DELETE FROM synced_episode WHERE episodeId = :id"
      [":id" := eeId]

getSyncedEpisodes :: DB [ExistingEpisode]
getSyncedEpisodes = do
  conn <- ask
  liftIO $ query_ conn "SELECT filename, episodeId FROM synced_episode"

createSyncedEpisodeTable :: DB ()
createSyncedEpisodeTable = do
  conn <- ask
  liftIO $
    execute_
      conn
      [r|
      CREATE TABLE IF NOT EXISTS synced_episode (
        episodeId INTEGER PRIMARY KEY NOT NULL,
        filename TEXT NOT NULL UNIQUE,
        addedAt INTEGER NOT NULL DEFAULT (strftime('%s', 'now'))
      )
    |]