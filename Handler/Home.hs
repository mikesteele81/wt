{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Import

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
    deleteForms <- mapM generateFormPost (deleteForm <$> map (resourceFilename . entityVal) resources)
    let deleteRows = zip resources deleteForms
        confirmRows = zipWith (\(Entity a b) (c, d) -> (a, b, c, d))
                      needsConfirmation confirmForms
    defaultLayout $ do
        setTitle "H3CWT - Main Page"
        $(widgetFile "homepage")

deleteForm :: Text -> Form Text
deleteForm name = renderDivs (areq hiddenField "" (Just name))

confirmForm :: Entity User -> Form UserId
confirmForm (Entity uid _) = renderDivs (areq hiddenField "" (Just uid))