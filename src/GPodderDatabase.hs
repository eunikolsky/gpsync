{-# LANGUAGE QuasiQuotes #-}

module GPodderDatabase
  ( getNewEpisodes
  , withDatabase
  ) where

import Control.Monad.Reader
import Database.SQLite.Simple
import Episode
import EpisodeDatabaseCompat ()
import Text.RawString.QQ

-- | Internal, opaque type to wrap the database `Connection`.
type DB a = ReaderT Connection IO a

{- | Wraps `withConnection` in order not to require other modules to import
`Database.SQLite.Simple`.
-}
withDatabase :: FilePath -> DB a -> IO a
withDatabase file = withConnection file . runReaderT

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
