{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Delete where

import Data.Text (Text)
import Filesystem as FS
import Filesystem.Path.CurrentOS as FS

import Yesod

import Foundation
import Model

postDeleteR :: Text -> Handler RepHtml
postDeleteR name = do
    Entity key res <- runDB $ getBy404 (UniqueResource name)
    liftIO . FS.removeFile . FS.fromText . resourceFilename $ res
    runDB $ delete key
    redirect HomeR
