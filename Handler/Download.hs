{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Download where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding as TLE
import Data.Time

import Data.Digest.Pure.SHA
import Network.HTTP.Types
import Yesod
import Yesod.Auth

import Foundation
import Model

getDownloadR :: Text -> Handler (ContentType, Content)
getDownloadR name = do
    req <- getRequest
    hmacKey <- getHmacKey <$> getYesod
    let mtimestampParam = lookup "timestamp" $ reqGetParams req
        msecretParam = lookup "secret" $ reqGetParams req
    case ((,) <$> mtimestampParam <*> msecretParam) of
      Nothing -> do
        -- We don't do auth handling declaritively for this route.
        _ <- requireAuthId
        timestamp <- liftIO $ T.pack . show <$> getCurrentTime
        let secret = genSecret hmacKey timestamp
        redirect ( DownloadR name, [ ("timestamp", timestamp)
                                   , ("secret", T.pack . show $ secret)])
      Just (timestampParam, secretParam) -> do
        --TODO: Verify secret
        let timestamp = read . T.unpack $ timestampParam
            secret = T.pack . show . genSecret hmacKey $ timestampParam
        now <- liftIO $ getCurrentTime
        if addUTCTime (fromInteger 3600) timestamp > now
          then do
            if secret == secretParam
              then do
                Entity _ res  <- runDB $ getBy404 $ UniqueResource name
                sendFile (TE.encodeUtf8 . resourceContentType $ res)
                    $ T.unpack . resourceFilename $ res
              else do
                redirectWith badRequest400 HomeR
          else do
            redirectWith gone410 HomeR
  where
    genSecret key = hmacSha1 key . TLE.encodeUtf8 . LT.fromStrict
