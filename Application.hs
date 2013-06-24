{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Control.Applicative

import Control.Monad.Logger (runLoggingT)
import Crypto.Random.AESCtr
import qualified Data.ByteString.Lazy as BL
import Database.Persist.Sql (runMigration)
import qualified Database.Persist
import Network.HTTP.Conduit (newManager, def)
import Network.Wai.Middleware.RequestLogger
import System.Log.FastLogger (mkLogger)
import System.IO (stdout)
import Yesod
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers

import Foundation
import Model
import Settings
import Settings.Development
import Settings.StaticFiles

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Changes
import Handler.Confirm
import Handler.Delete
import Handler.Download
import Handler.Home
import Handler.Publications
import Handler.Resource
import Handler.Resources
import Handler.User
import Handler.Users

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = Logger $ appLogger foundation
        }

    app <- toWaiAppPlain foundation
    return $ logWare app

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConfig)
    logger <- mkLogger True stdout
    (hmacKey, _) <- genRandomBytes 20 <$> makeSystem

    let foundation = App conf s p manager dbconf
                         (BL.fromChunks [hmacKey]) logger

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
