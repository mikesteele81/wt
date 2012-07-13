{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Resource where

import Control.Applicative
import Control.Monad (when)
import qualified Data.Text as T

import Filesystem
import Filesystem.Path.CurrentOS
import Yesod

import Foundation
import Model
import Settings (widgetFile)

getResourceR :: ResourceId -> Handler RepHtml
getResourceR rid = do
    res  <- runDB $ get404 rid
    (formWidget, formEncType) <- generateFormPost . editForm $ res
    publishForm <- generateFormPost $ renderDivs (areq hiddenField "" (Just rid))
    defaultLayout $ do
        setTitle . toHtml $ "H3CWT - " `T.append` resourceFilename res
        $(widgetFile "resource")

postResourceR :: ResourceId -> Handler RepHtml
postResourceR rid = do
    res <- runDB $ get404 rid
    ((result, _), _) <- runFormPost . editForm $ res
    case result of
      FormSuccess res' -> do
        let oldName = fromText $ resourceFilename res
        let newName = fromText $ resourceFilename res'
        let move = not $ oldName == newName
        when move $ liftIO $ copyFile oldName newName
        runDB $ do
            update rid
                [ ResourceFilename =. resourceFilename res'
                , ResourceFriendlyname =. resourceFriendlyname res'
                , ResourceContentType =. resourceContentType res'
                ]
            when move $ liftIO $ removeFile oldName
        setMessage $ "This resource has been updated."
      FormFailure ex -> setMessage . toHtml $ T.intercalate ", " ex
      _ -> return ()
    redirect $ ResourceR rid

editForm :: Resource -> Form Resource
editForm resource = renderDivs $ Resource
    <$> areq textField "filename" (Just . resourceFilename $ resource)
    <*> aopt textField "friendly name" (Just . resourceFriendlyname $ resource)
    <*> areq textField "MIME type" (Just . resourceContentType $ resource)
