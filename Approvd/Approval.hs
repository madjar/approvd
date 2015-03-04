{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
module Approvd.Approval (handlePR) where

import Approvd.Github
import Approvd.PullRequest
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V
import Text.Regex.PCRE
import Text.Shakespeare.Text

data Approval = Approval { apYes :: Bool
                         , apTotalComments :: Int
                         , apApprovingComments :: Int }

approval :: PullRequest -> Approval
approval (PullRequest {..}) = Approval {..}
  where apYes = apApprovingComments > 0
        apTotalComments = V.length prComments
        apApprovingComments = V.length $ mfilter commentApproves prComments


commentApproves :: Value -> Bool
commentApproves comment = body =~ pattern
  where pattern = "^[Aa]pprove?d" :: B.ByteString
        body = encodeUtf8 $ comment ^. key "body" . _String

approvalState :: Approval -> T.Text
approvalState (Approval True _ _) = "success"
approvalState _ = "pending"


approvalDescription :: Approval -> T.Text
approvalDescription (Approval {..}) =
  [st|Seen ${apTotalComments} relevant comments, with ${apApprovingComments} approvals.|]


handlePR :: Repo -> Int -> Github ()
handlePR repo prId =
  do pr <- pullRequest repo prId
     let appr = approval pr
         newState = approvalState appr
         newDescription = approvalDescription appr
     case (prState pr, prStDescription pr) of
       -- TODO: logging
       (Just s, Just d) | s == newState && d == newDescription -> return ()
       _ -> updateState pr newState newDescription
