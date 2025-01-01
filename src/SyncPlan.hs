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
import UnliftIO.Exception
import UnliftIO.IORef

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

data State = State {sFilepath :: !FilePath, sEpisodes :: !(IORef [ExistingEpisode])}

-- note: conceptualy, it should be a `StateT State IO`, but I have to use the
-- `ReaderT` so that it works with `unliftio`'s `bracket_`; standard `bracket`
-- only works with `IO` actions :(
type ExistingEpisodeStore = ReaderT State IO

withExistingEpisodeStore :: FilePath -> ExistingEpisodeStore a -> IO a
withExistingEpisodeStore fp f = do
  episodesRef <- newIORef mempty
  let initState = State{sFilepath = fp, sEpisodes = episodesRef}
  flip runReaderT initState $ bracket_ readEpisodes writeEpisodes f

getSyncedEpisodes :: ExistingEpisodeStore [ExistingEpisode]
getSyncedEpisodes = readIORef =<< asks sEpisodes

modify' :: ([ExistingEpisode] -> [ExistingEpisode]) -> ExistingEpisodeStore ()
modify' f = do
  ref <- asks sEpisodes
  atomicModifyIORef' ref $ \episodes -> (f episodes, ())

put :: [ExistingEpisode] -> ExistingEpisodeStore ()
put = modify' . const

addSyncedEpisode :: ExistingEpisode -> ExistingEpisodeStore ()
addSyncedEpisode episode = modify' replace
  where
    replace episodes = episode : without episode episodes

removeSyncedEpisode :: ExistingEpisode -> ExistingEpisodeStore ()
removeSyncedEpisode episode = modify' $ without episode

without :: ExistingEpisode -> [ExistingEpisode] -> [ExistingEpisode]
without episode = filter (\e -> eeId e /= eeId episode)

readEpisodes :: ExistingEpisodeStore ()
readEpisodes = do
  fp <- asks sFilepath
  ifM
    (liftIO $ doesFileExist fp)
    ( do
        Right vector <- C.decode C.NoHeader <$> liftIO (LBS.readFile fp)
        put $ V.toList vector
    )
    $ pure ()

writeEpisodes :: ExistingEpisodeStore ()
writeEpisodes = do
  fp <- asks sFilepath
  episodes <- getSyncedEpisodes
  let encoded = C.encode episodes
  liftIO $ LBS.writeFile fp encoded

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM mpred mtrue mfalse = do
  p <- mpred
  if p then mtrue else mfalse
