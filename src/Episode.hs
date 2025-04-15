module Episode (Episode (..), EpisodeId, TargetFilePath, targetFilePath) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import GHC.Generics
import System.FilePath
import Text.Show.Unicode

type EpisodeId = Int

{- | Regular episodes that are managed by gPodder. They are read from the
`episode + podcast` tables.
-}
data Episode = Episode
  -- it's important to sort by these two fields first
  { epPodcastTitle :: !Text
  , epPublishedAt :: !LocalTime
  , epId :: !EpisodeId
  , epEpisodeTitle :: !Text
  , epFilename :: !FilePath
  -- ^ path to file relative to gPodder's downloads dir
  }
  deriving stock (Eq, Ord, Generic)

instance Show Episode where
  show Episode{epId, epPodcastTitle, epEpisodeTitle, epFilename, epPublishedAt} =
    mconcat
      [ "Episode #"
      , show epId
      , " {podcast "
      , ushow epPodcastTitle
      , ", episode "
      , ushow epEpisodeTitle
      , ", filename "
      , ushow epFilename
      , ", published at "
      , show epPublishedAt
      , "}"
      ]

type TargetFilePath = FilePath

targetFilePath :: Episode -> TargetFilePath
targetFilePath Episode{epPodcastTitle, epEpisodeTitle, epFilename} =
  truncateFilename extension (process epPodcastTitle </> process epEpisodeTitle) <.> extension
  where
    extension = takeExtension epFilename
    process = T.unpack . sanitize
    -- TODO may need to sanitize other characters too
    -- https://github.com/gpodder/gpodder/blob/master/src/gpodder/util.py#L1658
    sanitize =
      T.replace "/" "∕" . T.replace ":" "᠄" . T.replace "\"" "❛" . T.replace "?" "⸮" . T.replace "|" "❘"

truncateFilename :: String -> FilePath -> FilePath
truncateFilename ext = withBytesView $ BS.take (240 - length ext)

{- | Applies function `f` to the bytes representation of the `FilePath`, making
sure that the result is still a valid UTF-8 string.
-}
withBytesView :: (ByteString -> ByteString) -> FilePath -> FilePath
withBytesView f = T.unpack . go . f . TE.encodeUtf8 . T.pack
  where
    go bs = case TE.decodeUtf8' bs of
      Right t -> t
      -- if decoding to utf-8 failed, we need to remove the last byte and try again
      Left _ -> go $ BS.init bs
