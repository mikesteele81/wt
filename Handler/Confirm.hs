{-# LANGUAGE OverloadedStrings #-}
module Handler.Confirm where

import qualified Data.Text as T
import Prelude

import Import

postConfirmR :: UserId -> Handler RepHtml
postConfirmR uid = do
    user <- runDB $ get404 uid
    if userConfirmed user
      then
        setMessage . toHtml
        $ T.concat [friendlyName user, " already has access."]
      else do   
        runDB $ update uid [UserConfirmed =. True]
        setMessage . toHtml
            $ T.concat [friendlyName user, " has been granted access."]
    redirect HomeR