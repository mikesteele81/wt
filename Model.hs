{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model where

import Data.Text as T

import Database.Persist.Quasi
import Yesod

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

-- |Don't check against DB for now.
friendlyName :: User -> Text
friendlyName (User (Just fn) (Just ln) _ _    ) = T.concat [fn, " ", ln]
friendlyName (User (Just fn) _         _ _    ) = fn
friendlyName (User _         (Just ln) _ _    ) = ln
friendlyName (User _         _         _ email) = email
