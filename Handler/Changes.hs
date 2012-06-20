{-# LANGUAGE OverloadedStrings #-}
module Handler.Changes where

import Import

getChangesR :: Handler RepHtml
getChangesR = do
    defaultLayout $ do
        setTitle "W3CWT - recent changes"
        $(widgetFile "changes")
