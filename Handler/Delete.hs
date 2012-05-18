{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Delete where

import Filesystem as FS
import Filesystem.Path.CurrentOS as FS

import Import

postDeleteR :: Text -> Handler RepHtml
postDeleteR name = do
    Entity key res <- runDB $ getBy404 (UniqueResource name)
    liftIO . FS.removeFile . FS.fromText . resourceFilename $ res
    runDB $ delete key
    redirect HomeR
