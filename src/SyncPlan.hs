module SyncPlan
  ( ExistingEpisode (..)
  , ExistingEpisodeStore
  , SyncAction (..)
  , addSyncedEpisode
  , getSyncPlan
  , getSyncedEpisodes
  , removeSyncedEpisode
  , withExistingEpisodeStore
  ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.ByteString.Lazy qualified as LBS
import Data.Csv qualified as C
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Vector qualified as V
import Episode
import GHC.Generics
import System.Directory
import Text.Show.Unicode

{- | Episodes that were previously synced. They are read from/written to the
`gpsync.csv` file in the gPodder's directory. The program assumes that this
file reflects the filesystem view on the player device, that is that only it
makes modifications to the sync directory on the device.
-}
data ExistingEpisode = ExistingEpisode
  { eeFilename :: !TargetFilePath
  , -- it would make sense to put the id first, but this way the compiler does the
    -- work by ordering the filename before the id
    eeId :: !EpisodeId
  }
  deriving stock (Eq, Ord, Generic)

instance Show ExistingEpisode where
  show ExistingEpisode{eeId, eeFilename} =
    mconcat
      [ "ExistingEpisode #"
      , show eeId
      , " {filename "
      , ushow eeFilename
      , "}"
      ]

instance C.FromRecord ExistingEpisode
instance C.ToRecord ExistingEpisode

data SyncAction = Delete !ExistingEpisode | Copy !Episode
  deriving stock (Show, Eq, Ord)

getSyncPlan :: [Episode] -> [ExistingEpisode] -> Set SyncAction
getSyncPlan newEpisodes existingEpisodes = toCopy <> toDelete
  where
    toCopy = S.fromList . fmap Copy . M.elems $ newM M.\\ existingM
    toDelete = S.fromList . fmap Delete . M.elems $ existingM M.\\ newM

    newM = M.fromList $ (\e -> (epId e, e)) <$> newEpisodes
    existingM = M.fromList $ (\e -> (eeId e, e)) <$> existingEpisodes

type ExistingEpisodeStore = ReaderT FilePath (StateT [ExistingEpisode] IO)

withExistingEpisodeStore :: FilePath -> ExistingEpisodeStore () -> IO ()
-- FIXME `bracket`? it requires `IO` actions, which aren't the types here…
withExistingEpisodeStore fp f = flip evalStateT mempty . flip runReaderT fp $ do
  readEpisodes
  f
  writeEpisodes

getSyncedEpisodes :: ExistingEpisodeStore [ExistingEpisode]
getSyncedEpisodes = get

addSyncedEpisode :: ExistingEpisode -> ExistingEpisodeStore ()
addSyncedEpisode episode = do
  modify' replace
  -- saving the episodes to disk after every addition because copying is slow
  -- and more likely to encounter errors than a removal
  writeEpisodes
  where
    replace episodes = episode : without episode episodes

removeSyncedEpisode :: ExistingEpisode -> ExistingEpisodeStore ()
removeSyncedEpisode episode = modify' $ without episode

without :: ExistingEpisode -> [ExistingEpisode] -> [ExistingEpisode]
without episode = filter (\e -> eeId e /= eeId episode)

readEpisodes :: ExistingEpisodeStore ()
readEpisodes = do
  fp <- ask
  ifM
    (liftIO $ doesFileExist fp)
    ( do
        Right vector <- C.decode C.NoHeader <$> liftIO (LBS.readFile fp)
        put $ V.toList vector
    )
    $ pure ()

writeEpisodes :: ExistingEpisodeStore ()
writeEpisodes = do
  fp <- ask
  encoded <- gets C.encode
  liftIO $ LBS.writeFile fp encoded

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM mpred mtrue mfalse = do
  p <- mpred
  if p then mtrue else mfalse
