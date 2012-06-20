module Handler.Download where

import qualified Data.Text as T

import Import

getDownloadR :: Text -> Handler (ContentType, Content)
getDownloadR name = do
    Entity _ res <- runDB $ getBy404 (UniqueResource name)
    sendFile typeOctet $ T.unpack . resourceFilename $ res
