{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Resources where

import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as Text

import Filesystem as FP
import Filesystem.Path.CurrentOS as FP

import Yesod
import Yesod.Core.Types (FileInfo (..))

import Foundation
import Model
import Settings (widgetFile)

getResourcesR :: Handler RepHtml
getResourcesR = do
    (formWidget, formEnctype) <- generateFormPost uploadForm
    defaultLayout $ do
        setTitle "W3CWT - Add a resource"
        $(widgetFile "addResourceForm")

postResourcesR :: Handler RepHtml
postResourcesR = do
    ((result, _), _) <- runFormPost uploadForm
    let submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    maybe (return ()) saveFile submission
    redirect HomeR

uploadForm :: Form (FileInfo, Maybe Text)
uploadForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> aopt textField "friendly name" Nothing

saveFile :: (FileInfo, Maybe Text) -> Handler ()
saveFile (FileInfo name contentType _ move, friendly) = runDB $ do
    _ <- insert $ Resource name friendly contentType
    liftIO $ move (Text.unpack name)
