{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.User where

import Control.Applicative
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Yesod

import Foundation
import Model
import Settings (widgetFile)

getUserR :: UserId -> Handler RepHtml
getUserR uid = do
    user <- runDB $ get404 uid
    let email = userPrimaryEmail user
    auths <- runDB $ catMaybes <$> sequence
        [ ifFound ("Google" :: Text) <$> getBy (UniqueGoogleAuth    email)
        , ifFound "BrowserID"        <$> getBy (UniqueBrowserIdAuth email)
        , ifFound "Facebook"         <$> getBy (UniqueFacebookAuthEmail  email)
        ]
    (formWidget, formEnctype) <- generateFormPost . updateForm $ user
    defaultLayout $ do
        setTitle . toHtml $ "H3CWT - " `T.append` userDisplayName user
        $(widgetFile "user")
  where
    ifFound x f = maybe Nothing (const $ Just x) f

postUserR :: UserId -> Handler RepHtml
postUserR uid = do
    user <- runDB $ get404 uid
    ((result, _), _) <- runFormPost . updateForm $ user
    case result of
      FormSuccess (mfn, mln) -> do
        runDB $ update uid [UserFirstName =. mfn, UserLastName =. mln]
        setMessage . toHtml $ userDisplayName user `T.append` " has been updated."
      -- I don't know what these errors look like
      FormFailure ex -> setMessage . toHtml $ T.intercalate ", " ex
      _ -> return ()
    redirect HomeR

updateForm :: User -> Form (Maybe Text, Maybe Text)
updateForm user = renderDivs $ (,)
    <$> aopt textField "first name" (Just . userFirstName $ user)
    <*> aopt textField "last name" (Just . userLastName $ user)
