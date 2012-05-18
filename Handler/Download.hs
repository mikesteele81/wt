{-# LANGUAGE OverloadedStrings #-}
module Handler.Download where

import qualified Data.Text as T

import Import

getDownloadR :: Text -> Handler RepHtml
getDownloadR name = do
    Entity _ res <- runDB $ getBy404 (UniqueResource name)
    _ <- sendFile typeOctet (T.unpack . resourceFilename $ res)
    redirect HomeR
