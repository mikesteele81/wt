module Handler.Temporary where

import qualified Data.Text as T
import Data.Time

import Network.HTTP.Types

import Import

-- Some agents such as the default Android browser pass download requests off
-- to 3rd party applications. To support these we redirect authenticated
-- download requests to temporary URLs which do _not_ require
-- authentication. Upon redirection a DB entry is created mapping the target
-- URL to a resource along with an expiration time.
-- Note: The second Text can be anything.
getTemporaryR :: Text -> Text -> Handler (ContentType, Content)
getTemporaryR hash _ = do
    -- perhaps the agent is using an old link
    Entity _ temp <- runDB $ getBy404 (UniqueTemporary hash)
    -- perhaps the associated resource no longer exists
    res  <- runDB $ get404 (temporaryResource temp)
    now <- liftIO $ getCurrentTime
    if (now > temporaryExpires temp)
      then redirectWith requestTimeout408 (ResourceR (resourceFilename res))
      else sendFile typeOctet $ T.unpack . resourceFilename $ res
