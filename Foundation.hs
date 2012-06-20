{-# LANGUAGE OverloadedStrings #-}

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

import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as S8
import Prelude

import qualified Data.Conduit as C
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Database.Persist.GenericSql
import qualified Database.Persist.Store
import qualified Facebook as FB
import Network.HTTP.Conduit (Manager, withManager)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Web.Authenticate.BrowserId
import Web.ClientSession (getKey)
import Yesod
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.Facebook
import qualified Yesod.Auth.Message as Msg
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

    isAuthorized route isWrite = do
        mauth <- maybeAuth
        runDB $ mauth `isAuthorizedTo` permissionsRequiredFor route isWrite

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
      | plugin == apName appGoogleEmailPlugin = runDB $ do
        mauth <- getBy $ UniqueGoogleAuth ident
        case mauth of
          -- This person has authenticated using Google before.
          Just (Entity _ auth) -> do
            muser <- getBy $ UniqueUser (googleAuthEmail auth)
            case muser of
              -- This GoogleAuth record is associated with a valid User record.
              Just (Entity uid _) -> return $ Just uid
              Nothing -> error "GoogleAuth record exists without a corresponding User record."
          -- Nobody has authenticated using Google and this email address.
          Nothing -> do
            muser <- getBy $ UniqueUser ident
            case muser of
              -- We already have a user account set up for this email address.
              -- Link them.
              Just (Entity uid _) -> do
                _ <- insert $ GoogleAuth ident uid
                return $ Just uid
              -- We've never seen this email address before. Create a new user
              -- account.
              Nothing -> do
                numUsers <- count ([] :: [Filter User])
                uid <- insert $ User (lookup "firstname" extra)
                    -- The first user needs to be confirmed.
                    (lookup "lastname" extra) Nothing (numUsers == 0) ident
                _ <- insert $ GoogleAuth ident uid
                return $ Just uid
      | plugin == apName appBrowserIdPlugin = runDB $ do
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
                uid <- insert $ User Nothing Nothing Nothing (numUsers == 0) ident
                _ <- insert $ BrowserIdAuth ident uid
                return $ Just uid
      | plugin == apName appFacebookPlugin = do
        mauth <- runDB . getBy $ UniqueFacebookAuth ident
        case mauth of
          -- This person has authenticated using Facebook before.
          Just (Entity _ auth) -> do
            muser <- runDB . getBy $ UniqueUser (facebookAuthEmail auth)
            case muser of
              -- This FacebookAuth record is associated with a valid User
              -- record.
              Just (Entity uid _) -> return $ Just uid
              Nothing -> error "FacebookAuth record exists without a corresponding User record."
          -- Nobody has authenticated using Facebook and this email address.
          Nothing -> do
            mtoken <- getUserAccessToken
            (mfirstName, mlastName, memail) <- withManager $ \mgr ->
                FB.runFacebookT Settings.fbCredentials mgr $ do
                    fbUser <- FB.getUser "me" [] mtoken
                    return ( FB.userFirstName fbUser, FB.userLastName fbUser
                           , FB.userEmail fbUser)
            case memail of
              Nothing -> error "unable to retreive email address from Facebook."
              Just email -> runDB $ do
                muser <- getBy $ UniqueUser email
                case muser of
                  -- We already have a user account set up for this email
                  -- address. Link them.
                  Just (Entity uid _) -> do
                    _ <- insert $ FacebookAuth ident email uid
                    return $ Just uid
                  -- We've never seen this email address before. Create a new
                  -- user account.
                  Nothing -> do
                    numUsers <- count ([] :: [Filter User])
                    uid <- insert $ User mfirstName mlastName
                                         -- The first user needs to be confirmed.
                                         Nothing (numUsers == 0) email
                    _ <- insert $ FacebookAuth ident email uid
                    return $ Just uid
      | otherwise = invalidArgs [plugin, ident]

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [ appFacebookPlugin
                    , appGoogleEmailPlugin
                    , appBrowserIdPlugin]

    loginHandler = defaultLayout $ do
        setTitle "H3CWT - Login"
        tm <- lift getRouteToMaster
        master <- lift getYesod
        let loginWidgets = map (flip apLogin tm) (authPlugins master)
        $(widgetFile "login")

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

-- Taken from yesod-auth-fb, and modified so my own logo is displayed.
appFacebookPlugin :: AuthPlugin App
appFacebookPlugin = (authFacebook Settings.fbCredentials ["email"])
    { apLogin = \tm -> do
          redirectUrl <- lift (getRedirectUrl tm)
          [whamlet|
<a href="#{redirectUrl}">
  <img src=@{StaticR img_facebook_logo_png} alt=_{Msg.Facebook}>
|]
    }
  where
    -- Run a Facebook action.
    runFB :: YesodAuth master =>
             FB.FacebookT FB.Auth (C.ResourceT IO) a
          -> GHandler sub master a
    runFB act = do
      manager <- authHttpManager <$> getYesod
      liftIO $ C.runResourceT $ FB.runFacebookT Settings.fbCredentials manager act

    getRedirectUrl :: YesodAuth master =>
                      (Route Auth -> Route master)
                   -> GHandler sub master Text
    getRedirectUrl tm = do
        render  <- getUrlRender
        let proceedUrl = render (tm proceedR)
        runFB $ FB.getUserAccessTokenStep1 proceedUrl ["email"]
    proceedR = PluginR "fb" ["proceed"]

-- Taken from yesod-auth, and modified so that my own logo is displayed.
appGoogleEmailPlugin :: AuthPlugin App
appGoogleEmailPlugin = authGoogleEmail
    { apLogin = \tm -> do
        [whamlet|
<a href=@{tm forwardUrl}>
  <img src=@{StaticR img_google_logo_png} alt=_{Msg.LoginGoogle}>
|]
    }

-- Taken from yesod-auth, and modified so that my own logo is displayed.
appBrowserIdPlugin :: AuthPlugin App
appBrowserIdPlugin = authBrowserId
    { apLogin = \toMaster -> do
        addScriptRemote browserIdJs
        toWidget [hamlet|
<a href="javascript:navigator.id.getVerifiedEmail(function(a){if(a)document.location='@{toMaster complete}/'+a});">
  <img src=@{StaticR img_browserid_logo_png} alt="BrowserId">
|]
    }
  where
    complete = PluginR "browserid" []

data Permission = EditProfile UserId
                | Other

permissionsRequiredFor :: Route App -> Bool -> [Permission]
permissionsRequiredFor (UserR uid) True = [EditProfile uid]
permissionsRequiredFor (AuthR _)   _    = []
permissionsRequiredFor RobotsR     _    = []
permissionsRequiredFor FaviconR    _    = []
permissionsRequiredFor (StaticR _) _    = []
permissionsRequiredFor ChangesR    _    = []
permissionsRequiredFor _           _    = [Other]

hasPermissionTo :: Entity User -> Permission -> YesodDB sub App AuthResult
hasPermissionTo (Entity uid  _) (EditProfile uid') = return $
    if uid == uid'
    then Authorized
    else Unauthorized "Sorry, but I cannot let you modify someone else's profile."
hasPermissionTo (Entity _ user) Other = return $
    if userConfirmed user
    then Authorized
    else Unauthorized "Sorry, but I'm waiting for an existing member to confirm your access. Once this has been done you will be able to use the service. Please try again later."

isAuthorizedTo :: Maybe (Entity User) -> [Permission]
    -> YesodDB sub App AuthResult
isAuthorizedTo _        []     = return Authorized
isAuthorizedTo Nothing  _      = return AuthenticationRequired
isAuthorizedTo (Just u) (p:ps) = do
    r <- hasPermissionTo u p
    case r of
      Authorized -> Just u `isAuthorizedTo` ps
      _          -> return r -- unauthorized
