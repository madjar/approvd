{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Approvd.PullRequest (PullRequest(..), Repo(..), pullRequest, updateState) where

import Approvd.Github
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import Data.Foldable
import qualified Data.Text as T

import Debug.Trace

context :: T.Text
context = "approvd"

-- TODO: use a real type for the comments
data PullRequest = PullRequest { prRepo :: Repo
                               , prTitle :: T.Text
                               , prNum :: Int
                               , prSha :: String
                               , prComments :: Array
                               , prState :: Maybe T.Text
                               , prStDescription :: Maybe T.Text}

data Repo = Repo String String


repoUrl :: Repo -> [String] -> [String]
repoUrl (Repo user repo) rest = ["repos", user, repo] ++ rest

pullRequest :: Repo -> Int -> Github PullRequest
pullRequest prRepo prNum =
  do -- Get basic PR info
     pr <- get $ repoUrl prRepo ["pulls", show prNum]
     let sha = pr ^. key "head" . key "sha" . _String
         prSha = T.unpack sha
         prTitle = pr ^. key "title" . _String

     -- List relevant comments (that where posted after the last commit)
     pushDate <- commitPushDate prRepo sha -- TODO: parse as real date
     allComments <- get $ repoUrl prRepo ["issues", show prNum, "comments"]
     let isRelevant comment = comment ^. key "created_at" . _String > pushDate
         prComments = mfilter isRelevant (allComments ^. _Array)

     -- Get the current status
     status <- get $ repoUrl prRepo ["status", prSha]
     let ourState = find ours $ status ^. key "statuses" . _Array
                    where ours s = s ^. key "context" . _String == context
         prState = ourState <&> (^. key "state" . _String)
         prStDescription = ourState <&> (^. key "description" . _String)
     return $ PullRequest {..}

-- Update the state of a pull request
updateState :: PullRequest -> T.Text -> T.Text -> Github ()
updateState pr state description = post url payload >> return ()
  where url = repoUrl (prRepo pr) ["statuses", prSha pr]
        payload = object [ "state" .= state
                         , "description" .=  description
                         , "context" .= context]

-- To known when a commit was pushed, I should have a webhook and listen for pull_request events with the synchronized action. Otherwise, I have to resort to the following hack.
commitPushDate :: Repo -> T.Text -> Github T.Text
commitPushDate repo sha =
  do events <- get $ repoUrl repo ["events"]
     let event = find f (events ^. _Array)
     case event of
      Just e -> return $ e ^. key "created_at" . _String
      -- TODO: real logging
      Nothing -> trace ("XXX: No date found for " ++ T.unpack sha) $ return ""
  where f event = isPushEvent event && eventSha event == sha
        isPushEvent event = event ^. key "type" . _String == "PushEvent"
        eventSha event = event ^. key "payload" . key "head" . _String
