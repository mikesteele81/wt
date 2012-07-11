module Handler.Resource where

import Data.Text (Text)
import qualified Data.Text as T

import Yesod

import Foundation
import Model

-- some clients pass off download requests to other applications. These will
-- fail to authenticate. To work around this, all download requests redirect
-- to a temporary url which does not require authentication.
getResourceR :: Text -> Handler (ContentType, Content)
getResourceR name = do
    Entity _ res  <- runDB $ getBy404 $ UniqueResource name
    sendFile typeOctet $ T.unpack . resourceFilename $ res
