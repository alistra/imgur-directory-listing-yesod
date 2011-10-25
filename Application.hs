{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( withImgurDirectoryListing
    , withDevelAppPort
    ) where

import Foundation
import Settings
import Yesod.Static
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Yesod.Logger (Logger)
import Data.Dynamic (Dynamic, toDyn)
import qualified Database.Persist.Base
import Database.Persist.GenericSql (runMigration)
import Control.Concurrent
import HinotifyHandler (handler, registerHinotify, void)

-- Import all relevant handler modules here.
import Handler.Root

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "ImgurDirectoryListing" resourcesImgurDirectoryListing

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withImgurDirectoryListing :: AppConfig DefaultEnv -> Logger -> (Application -> IO ()) -> IO ()
withImgurDirectoryListing conf logger f = do
#ifdef PRODUCTION
    s <- static Settings.staticDir
#else
    s <- staticDevel Settings.staticDir
#endif
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
            $ either error return . Database.Persist.Base.loadConfig

    Database.Persist.Base.withPool (dbconf :: Settings.PersistConfig) $ \p -> do
        Database.Persist.Base.runPool dbconf (runMigration migrateAll ) p
        let h = ImgurDirectoryListing conf logger s p
        liftIO $ void $ forkIO $ registerHinotify (hinotifyHandler dbconf)
        defaultRunner f h
    where
        -- | Event handler for INotify
        hinotifyHandler dbconf e = do
            Database.Persist.Base.withPool (dbconf :: Settings.PersistConfig) $ void . Database.Persist.Base.runPool dbconf (handler e)

-- for yesod devel
withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelApp withImgurDirectoryListing
