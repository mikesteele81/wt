module Handler.Resource where

import Data.Time
import qualified Data.Text as T

import Network.HTTP.Types

import Import

-- some clients pass off download requests to other applications. These will
-- fail to authenticate. To work around this, all download requests redirect
-- to a temporary url which does not require authentication.
getResourceR :: Text -> Handler (ContentType, Content)
getResourceR name = do
    Entity rid _ <- runDB $ getBy404 (UniqueResource name)
    now <- liftIO getCurrentTime
    let hash = T.pack . filter (not . flip elem " -:.UTC") . show $ now
        expires = addUTCTime (fromIntegral oneHour) now
    _ <- runDB . insert $ Temporary hash expires rid
    cacheSeconds oneHour
    redirectWith found302 (TemporaryR hash name)
  where
    oneHour = 3600 :: Int -- seconds
