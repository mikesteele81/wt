{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Changes where

import Yesod

import Foundation
import Settings (widgetFile)

getChangesR :: Handler RepHtml
getChangesR = do
    defaultLayout $ do
        setTitle "W3CWT - recent changes"
        $(widgetFile "changes")
