{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Settings where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Text (Text)

import Data.Yaml
import Database.Persist.Sqlite (SqliteConf)
import Facebook
import Language.Haskell.TH.Syntax
import Text.Shakespeare.Text (st)
import Yesod.Default.Config
import qualified Yesod.Default.Util

import Settings.Development

-- | Which Persistent backend this site is using.
type PersistConfig = SqliteConf

-- Static setting below. Changing these requires a recompile

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticDir :: FilePath
staticDir = "static"

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in Foundation.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Foundation.hs
staticRoot :: AppConfig DefaultEnv x -> Text
staticRoot conf = [st|#{appRoot conf}/static|]


-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = if development then Yesod.Default.Util.widgetFileReload
                            else Yesod.Default.Util.widgetFileNoReload

data Extra = Extra
    { extraCopyright :: Text
    , extraAnalytics :: Maybe Text -- ^ Google Analytics
    } deriving Show

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = Extra
    <$> o .:  "copyright"
    <*> o .:? "analytics"

fbCredentials :: Credentials
fbCredentials = Credentials
    undefined
    undefined
    undefined

sesAccessKey :: ByteString
sesAccessKey = undefined

sesSecretKey :: ByteString
sesSecretKey = undefined

sesFromAddress :: Text
sesFromAddress = undefined
