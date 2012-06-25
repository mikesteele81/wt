{-# LANGUAGE OverloadedStrings #-}

module Handler.Users where

import qualified Data.Text as T

import Import

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
    ((result, _), _) <- runFormPost addUserForm
    case result of
      FormSuccess user -> do
        muser <- runDB $ getBy $ UniqueUser (userPrimaryEmail user)
        case muser of
          Just (Entity _ existingUser) ->
            setMessage . toHtml $ friendlyName existingUser `T.append` " already exists."
          Nothing -> do
            _ <- runDB $ insert user
            setMessage . toHtml $ friendlyName user `T.append` " was added."
      FormFailure ex -> setMessage . toHtml . T.intercalate "\n" $ ex
      FormMissing -> return ()
    redirect UsersR

addUserForm :: Form User
addUserForm = renderDivs $ User
    <$> aopt textField "first name" Nothing
    <*> aopt textField "last name" Nothing
    -- Explicitly added users are already confirmed. After all, an existing
    -- confirmed user is typing this in.
    <*> pure True
    <*> areq textField "email address" Nothing
