{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Publications where

import Control.Exception (try)
import Data.Text (Text)
import Data.Text.Encoding as E

import Network.HTTP.Conduit (withManager, HttpException(..))
import Network.Mail.Mime
import Network.Mail.Mime.SES
import Text.Blaze.Html.Renderer.Utf8
import Text.Hamlet
import Yesod
import Yesod.Auth (requireAuth)

import Foundation
import Model
import qualified Settings

postPublicationsR :: ResourceId -> Handler RepHtml
postPublicationsR rid = do
    Entity _ publisher <- requireAuth
    res <- runDB $ get404 rid
    userEntities <- (runDB $ selectList ([] :: [Filter User]) [])
    let users = map entityVal userEntities
    render <- getUrlRenderParams
    liftIO $ mapM_ (sendNotification publisher res render) users
    setMessage "Email notifications have been sent out."
    redirect $ ResourceR rid

sendNotification :: User -> Resource -> (Route App -> [(Text, Text)] -> Text) -> User -> IO Bool
sendNotification publisher resource render user = do
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
                , mailHeaders = [("Subject", "New content has been uploaded")]
                , mailParts   = [[content]] }
    content = Part { partType     = "text/html; charset=\"utf-8\""
                   , partEncoding = QuotedPrintableText
                   , partFilename = Nothing
                   , partHeaders  = []
                   , partContent  = renderHtml $ $(hamletFile "templates/email-upload.hamlet") render
                   }
    toAddress = userPrimaryEmail user
