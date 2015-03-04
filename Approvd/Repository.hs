{-# LANGUAGE OverloadedStrings #-}
module Approvd.Repository where

import Approvd.Github

import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector

data Repository = Repository { rFullName :: Text
                             , rDescription :: Text
                             , rEnabled :: Bool
                             }
                             deriving (Show)

-- TODO: I'll need pagination
getRepos :: Text -> Github (Vector Repository)
getRepos payloadUrl = do repos <- get ["user", "repos"]
                         traverse toRepo . mfilter isAdmin $ repos ^. _Array
  where isAdmin r = r ^? key "permissions" . key "admin" . _Bool == Just True
        toRepo r = do let fullName = r ^. key "full_name" . _String
                      hooks <- get ["repos", T.unpack fullName, "hooks"]
                      let enabled = anyOf (_Array . folded . key "config" . key "url" . _String)
                                          (== payloadUrl)
                                          hooks
                      return $ Repository fullName
                                          (r ^. key "description" . _String)
                                          enabled

activateHook :: Text -> Text -> Github ()
activateHook payloadUrl repo = void $ post ["repos", T.unpack repo, "hooks"] payload
  where payload = object ["name" .= String "web"
                         ,"events" .= [String "issue_comment"]
                         ,"config" .= object ["url" .= payloadUrl
                                             ,"content_type" .= String "json"
                                             ,"secret" .= String ""
                                             ,"insecure_ssl" .= String "0"]]
