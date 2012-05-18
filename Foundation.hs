module Foundation
    ( App (..)
    , Route (..)
    , AppMessage (..)
    , resourcesApp
    , Handler
    , Widget
    , Form
    , maybeAuth
    , requireAuth
    , module Settings
    , module Model
    ) where

import qualified Data.ByteString.Char8 as S8
import Prelude

import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Database.Persist.GenericSql
import qualified Database.Persist.Store
import Network.HTTP.Conduit (Manager)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Yesod
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logMsg, formatLogText)
import qualified Network.Wai as W
import Yesod.Static

import GoogleEmail
import Model
import qualified Settings
import Settings (widgetFile, Extra (..))
import Settings.StaticFiles

-- | The site argument for your application. This can be a good place to
--   keep settings and values requiring initialization before your
--   application starts running, such as database connections. Every handler
--   will have access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm App App (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 120

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        -- We can't use requireAuth here since defaultLayout is used by
        -- error responses.
        mauth <- maybeAuth

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    errorHandler = wtErrorHandler

    isAuthorized (AuthR _)   _ = return Authorized
    isAuthorized RobotsR     _ = return Authorized
    isAuthorized FaviconR    _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized _           _ = do
        mauth <- maybeAuth
        return $ case mauth of
          Nothing -> AuthenticationRequired
          Just (Entity _ user) -> if userConfirmed user then Authorized
                                  else Unauthorized "Sorry, but I'm waiting for an existing member to confirm your access. Once this has been done you will be able to use the service. Please try again later."

    maximumContentLength _ _ = 50 * 1024 * 1024 -- 50 MiB

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    messageLogger y loc level msg =
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page
    -- loads first
    jsLoader _ = BottomOfBody

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId (Creds plugin ident extra)
      | plugin == apName (authGoogleEmail :: AuthPlugin App) = runDB $ do
        mauth <- getBy $ UniqueGoogleAuth ident
        case mauth of
          Just (Entity _ auth) -> do
            muser <- getBy $ UniqueUser (googleAuthEmail auth)
            case muser of
              Just (Entity uid _) -> return $ Just uid
              Nothing -> error "GoogleAuth record exists without a corresponding User record."
          Nothing -> do
            muser <- getBy $ UniqueUser ident
            case muser of
              Just (Entity uid _) -> do
                _ <- insert $ GoogleAuth ident uid
                return $ Just uid
              Nothing -> do
                numUsers <- count ([] :: [Filter User])
                -- The first user needs to be confirmed.
                let confirmed = if numUsers == 0 then True else False
                uid <- insert $ User (lookup "firstname" extra)
                    (lookup "lastname" extra) Nothing confirmed ident
                _ <- insert $ GoogleAuth ident uid
                return $ Just uid
      | plugin == apName (authBrowserId :: AuthPlugin App) = runDB $ do
        mauth <- getBy $ UniqueBrowserIdAuth ident
        case mauth of
          Just (Entity _ auth) -> do
            muser <- getBy $ UniqueUser (browserIdAuthEmail auth)
            case muser of
              Just (Entity uid _) -> return $ Just uid
              Nothing -> error "BrowserIdAuth record exists without a corresponding User record."
          Nothing -> do
            muser <- getBy $ UniqueUser ident
            case muser of
              Just (Entity uid _) -> do
                _ <- insert $ BrowserIdAuth ident uid
                return $ Just uid
              Nothing -> do
                numUsers <- count ([] :: [Filter User])
                -- The first user needs to be confirmed.
                let confirmed = if numUsers == 0 then True else False
                uid <- insert $ User Nothing Nothing Nothing confirmed ident
                _ <- insert $ BrowserIdAuth ident uid
                return $ Just uid
      | otherwise = invalidArgs [plugin, ident]

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authBrowserId, authGoogleEmail]

    authHttpManager = httpManager

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

-- | modified from yesod-core's implementation.
wtErrorHandler :: Yesod y => ErrorResponse -> GHandler sub y ChooseRep
wtErrorHandler NotFound = do
    r <- waiRequest
    let path' = TE.decodeUtf8With TEE.lenientDecode $ W.rawPathInfo r
    applyLayout' "Not Found"
        [hamlet|
<h1>Not Found
<p>#{path'}
|]
wtErrorHandler (PermissionDenied msg) =
    applyLayout' "Permission Denied"
        [hamlet|
<h2>Permission denied
<p>#{msg}
|]
wtErrorHandler (InvalidArgs ia) =
    applyLayout' "Invalid Arguments"
        [hamlet|
<h1>Invalid Arguments
<ul>
    $forall msg <- ia
        <li>#{msg}
|]
wtErrorHandler (InternalError e) =
    applyLayout' "Internal Server Error"
        [hamlet|
<h1>Internal Server Error
<p>#{e}
|]
wtErrorHandler (BadMethod m) =
    applyLayout' "Bad Method"
        [hamlet|
<h1>Method Not Supported
<p>Method "#{S8.unpack m}" not supported
|]

applyLayout' :: Yesod master
             => Html -- ^ title
             -> HtmlUrl (Route master) -- ^ body
             -> GHandler sub master ChooseRep
applyLayout' title body = fmap chooseRep $ defaultLayout $ do
    setTitle title
    toWidget body
