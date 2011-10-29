module HinotifyHandler where

import Foundation
import Network.Imgurder
import Network.URL
import System.INotify
import qualified Data.Text as T
import Control.Monad.IO.Class(MonadIO)
import Data.Time
import System.FilePath.Posix

-- | Registers INotify watchers
registerHinotify :: Control.Monad.IO.Class.MonadIO m => (Event -> IO ()) -> m ()
registerHinotify h = do
    inotify <- liftIO initINotify
    void $ liftIO $ addWatch inotify [MoveIn, MoveOut, Create, Delete] imgdir h

-- | Handler in a PersistBackend monad
handler :: PersistBackend b m => Event -> b m ()
handler (MovedIn  False fp _) = add fp
handler (MovedOut False fp _) = del fp
handler (Created False fp) = add fp
handler (Deleted False fp) = del fp
handler _ = return ()

-- | Imgur API key
key :: String
key = "27b25626f64a6a77fea07ec3ad2d5250"

-- | Image directory
imgdir :: FilePath
imgdir = "/images"

-- | Add an image to imgur.com and the database
add ::  PersistBackend b m => String -> b m ()
add fp = do
    eitherIU <- liftIO $ upload key (imgdir </> fp)
    case eitherIU of
        Left _ -> return ()
        Right (ImgurUpload _ _ link bthumb sthumb _  dellink) -> do
            date <- liftIO getCurrentTime
            void $ insert $ Image (T.pack fp) (T.pack $ exportURL link) (T.pack $ exportURL bthumb) (T.pack $ exportURL sthumb) (T.pack $ exportURL dellink) date
        Right (ImgurFailure _)-> return ()

-- | Remove an image from the database
del ::  PersistBackend b m => String -> b m ()
del fp = deleteBy $ UniqueFilename (T.pack fp)

void :: Monad m => m a -> m ()
void a = a >> return ()
