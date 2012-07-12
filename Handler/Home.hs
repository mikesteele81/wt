{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Home where

import Control.Applicative

import Yesod
import Yesod.Auth (maybeAuth)

import Foundation
import Model
import Settings (widgetFile)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    mauth <- maybeAuth
    resources <- runDB $ selectList ([] :: [Filter Resource]) []
    needsConfirmation <- runDB $ selectList [UserConfirmed ==. False] []
    confirmForms <- mapM generateFormPost (confirmForm <$> needsConfirmation)
    let confirmRows = zipWith (\(Entity a b) (c, d) -> (a, b, c, d))
                      needsConfirmation confirmForms
    defaultLayout $ do
        setTitle "H3CWT - Main Page"
        $(widgetFile "homepage")

confirmForm :: Entity User -> Form UserId
confirmForm (Entity uid _) = renderDivs (areq hiddenField "" (Just uid))