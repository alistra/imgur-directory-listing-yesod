module HinotifyHandler where

import Foundation
import Imgurder
import System.INotify
import qualified Data.Text as T
import Control.Monad.IO.Class(MonadIO)


-- | Registers INotify watchers
registerHinotify :: Control.Monad.IO.Class.MonadIO m => (Event -> IO ()) -> m ()
registerHinotify h = do
    inotify <- liftIO $ initINotify
    void $ liftIO $ addWatch inotify [MoveIn, MoveOut, Create, Delete] "/images" h

-- | Handler in a PersistBackend monad
handler :: PersistBackend b m => Event -> b m ()
handler (MovedIn  False fp _) = add fp
handler (MovedOut False fp _) = del fp
handler (Created False fp) = add fp
handler (Deleted False fp) = del fp
handler _ = return ()

-- | Add an image to imgur.com and the database
add ::  PersistBackend b m => [Char] -> b m ()
add fp = do
    maybeIU <- liftIO $ upload ("/images/" ++ fp)
    case maybeIU of
        Nothing -> return ()
        Just (ImgurUpload _ _ link bthumb sthumb _  dellink) -> void $ insert $ Image (T.pack fp) (T.pack link) (T.pack bthumb) (T.pack sthumb) (T.pack dellink)

-- | Remove an image from the database
del ::  PersistBackend b m => String -> b m ()
del fp = deleteBy $ UniqueFilename (T.pack fp)

void :: Monad m => m a -> m ()
void a = a >> return ()
