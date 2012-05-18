{-# LANGUAGE OverloadedStrings #-}
module Handler.Add where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Filesystem as FP
import Filesystem.Path.CurrentOS as FP

import Import

getAddR :: Handler RepHtml
getAddR = do
    (formWidget, formEnctype) <- generateFormPost uploadForm
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "addform")

postAddR :: Handler RepHtml
postAddR = do
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
saveFile (FileInfo name contentType content, friendly) = runDB $ do
    _ <- insert $ Resource name friendly contentType
    liftIO $ FP.writeFile (FP.fromText name) $ BS.pack . BL.unpack $ content
