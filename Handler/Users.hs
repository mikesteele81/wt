{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Users where

import Control.Applicative
import Control.Exception (try)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding as E

import Network.HTTP.Conduit (withManager, HttpException (..))
import Network.Mail.Mime
import Network.Mail.Mime.SES
import Text.Blaze.Html.Renderer.Utf8
import Text.Hamlet
import Yesod
import Yesod.Auth (requireAuth)

import Foundation
import Model

import Settings (widgetFile)
import qualified Settings

getUsersR :: Handler RepHtml
getUsersR = do
    users                     <- runDB $ selectList [] []
    facebookAuths             <- runDB $ selectList [] []
    googleAuths               <- runDB $ selectList [] []
    browserIdAuths            <- runDB $ selectList [] []
    (formWidget, formEnctype) <- generateFormPost addUserForm
    defaultLayout $ do
        setTitle "H3CWT - List users"
        $(widgetFile "users")

postUsersR :: Handler RepHtml
postUsersR = do
    Entity _ thisUser <- requireAuth
    ((result, _), _) <- runFormPost addUserForm
    case result of
      FormSuccess (newUser, notify) -> do
        mexistingUser <- runDB $ getBy $ UniqueUser (userPrimaryEmail newUser)
        case mexistingUser of
          Just (Entity _ existingUser) ->
            setMessage . toHtml $ userDisplayName existingUser `T.append` " already exists."
          Nothing -> do
            _ <- runDB $ insert newUser
            when notify $ do
                render <- getUrlRenderParams
                liftIO $ sendWelcomeMail thisUser render newUser >> return ()
            setMessage . toHtml $ userDisplayName newUser `T.append` " was added."
      FormFailure ex -> setMessage . toHtml . T.intercalate "\n" $ ex
      FormMissing -> return ()
    redirect UsersR

addUserForm :: Form (User, Bool)
addUserForm = renderDivs $ (,)
    <$> (User <$> aopt textField "first name" Nothing
              <*> aopt textField "last name" Nothing
              -- Explicitly added users are already confirmed. After all, an existing
              -- confirmed user is typing this in.
              <*> pure True
              <*> areq textField "email address" Nothing)
    <*> ( maybe False id <$>
          aopt checkBoxField "Send notification email message" (Just $ Just True))

sendWelcomeMail :: User -> (Route App -> [(Text, Text)] -> Text) -> User -> IO Bool
sendWelcomeMail addingUser render user = do
    eres <- try (withManager $ \manager -> renderSendMailSES manager ses mail)
    return $ either (const False) (const True) (eres :: Either HttpException ())
  where
    ses = SES { sesFrom      = E.encodeUtf8 Settings.sesFromAddress
              , sesTo        = [E.encodeUtf8 toAddress]
              , sesAccessKey = Settings.sesAccessKey
              , sesSecretKey = Settings.sesSecretKey }
    mail = Mail { mailFrom    = Address (Just "Worship Team") Settings.sesFromAddress
                , mailTo      = [Address Nothing toAddress]
                , mailCc      = []
                , mailBcc     = []
                , mailHeaders = [("Subject", "Welcome to the the worship team web service")]
                , mailParts   = [[content]] }
    content = Part { partType     = "text/html; charset=\"utf-8\""
                   , partEncoding = QuotedPrintableText
                   , partFilename = Nothing
                   , partHeaders  = []
                   , partContent  = renderHtml $ $(hamletFile "templates/welcome-email.hamlet") render
                   }
    toAddress = userPrimaryEmail user
