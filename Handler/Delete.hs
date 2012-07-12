{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Handler.Delete where

import Filesystem as FS
import Filesystem.Path.CurrentOS as FS

import Yesod

import Foundation
import Model
import Settings (widgetFile)

getDeleteR :: ResourceId -> Handler RepHtml
getDeleteR rid = do
    (formWidget, formEncType) <- generateFormPost $ deleteForm rid
    defaultLayout $ do
        setTitle $ "H3CWT - Removal confirmation"
        $(widgetFile "confirmDelete")

postDeleteR :: ResourceId -> Handler RepHtml
postDeleteR rid = do
    res <- runDB $ get404 rid
    liftIO . FS.removeFile . FS.fromText . resourceFilename $ res
    runDB $ delete rid
    redirect HomeR

deleteForm :: ResourceId -> Form ResourceId
deleteForm rid = renderDivs (areq hiddenField "" (Just rid))
