module Handler.Users where

import Import

getUsersR :: Handler RepHtml
getUsersR = do
    users <- runDB $ selectList [] []
    emails <- runDB $ selectList [] []
    googleAuths <- runDB $ selectList [] []
    browserIdAuths <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "users")
