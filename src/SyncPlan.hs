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

import Control.Exception
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

type ExistingEpisodeStore = StateT [ExistingEpisode] IO

withExistingEpisodeStore :: FilePath -> ExistingEpisodeStore a -> IO a
withExistingEpisodeStore fp f =
  bracket
    -- note: I had to make `readEpisodes` and `writeEpisodes` work in `IO` instead
    -- of `ExistingEpisodeStore` to satisfy `bracket`'s types; an alternative only
    -- seems to be `unliftio`'s `bracket`, but then I'd need to replace the
    -- `StateT x IO` with a `ReaderT (IORef x) IO`
    (readEpisodes fp)
    (writeEpisodes fp)
    (evalStateT f)

getSyncedEpisodes :: ExistingEpisodeStore [ExistingEpisode]
getSyncedEpisodes = get

addSyncedEpisode :: ExistingEpisode -> ExistingEpisodeStore ()
addSyncedEpisode episode = modify' replace
  where
    replace episodes = episode : without episode episodes

removeSyncedEpisode :: ExistingEpisode -> ExistingEpisodeStore ()
removeSyncedEpisode episode = modify' $ without episode

without :: ExistingEpisode -> [ExistingEpisode] -> [ExistingEpisode]
without episode = filter (\e -> eeId e /= eeId episode)

readEpisodes :: FilePath -> IO [ExistingEpisode]
readEpisodes fp = do
  fpExists <- doesFileExist fp
  if fpExists
    then do
      Right vector <- C.decode C.NoHeader <$> LBS.readFile fp
      pure $ V.toList vector
    else pure mempty

writeEpisodes :: FilePath -> [ExistingEpisode] -> IO ()
writeEpisodes fp episodes = do
  let encoded = C.encode episodes
  LBS.writeFile fp encoded
