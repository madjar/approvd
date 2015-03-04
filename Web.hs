{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web where

import Approvd.Approval
import Approvd.Github (runGithub)
import Approvd.Repository

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe (fromJust)
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable
import Database.Persist
import Database.Persist.Redis
import Database.Persist.TH
import Language.Haskell.TH.Syntax
import Network.HTTP.Client.Conduit (Manager, newManager)
import Text.Hamlet (hamletFile)
import Yesod
import Yesod.Auth
import Yesod.Auth.OAuth2.Github

-- | Define our database models
share [mkPersist (mkPersistSettings (ConT ''RedisBackend))] [persistLowerCase|
User
    email Text
    login Text
    avatarUrl Text
    accessToken Text
    name Text
    deriving Show Typeable
|]

-- | The foundation type
data Approvd = Approvd { httpManager :: Manager
                       , redisPool :: Connection}

-- | A nice constructor for the foundation
makeFoundation :: IO Approvd
makeFoundation = Approvd <$> newManager <*> createPoolConfig redisConfig
  where redisConfig = RedisConf "localhost" (PortNumber 6379) Nothing connCount
        connCount = 10

-- | The routes of the application
mkYesod "Approvd" [parseRoutes|
/ HomeR GET
/repos RepoR GET POST
/payload PayloadR POST
/auth AuthR Auth getAuth
|]

-- | Make the foundation type work with yesod
instance Yesod Approvd where
    approot = ApprootStatic "http://approvd-georges.ngrok.com"

    authRoute _ = Just $ AuthR LoginR

    defaultLayout w = do
        p <- widgetToPageContent w
        mmsg <- getMessage
        withUrlRenderer $(hamletFile "templates/layout.hamlet")

-- | Add persistence
instance YesodPersist Approvd where
    type YesodPersistBackend Approvd = RedisBackend

    runDB action = do master <- getYesod
                      runRedisPool action $ redisPool master

-- | Add authentication
instance YesodAuth Approvd where
    type AuthId Approvd = UserId

    getAuthId creds = runDB $ do
      redisKey <- either (fail . unpack) return $ keyFromValues [PersistText $ credsIdent creds] 
      x <- get redisKey :: RedisT Handler (Maybe User)
      case x of
        Just _ -> return $ Just redisKey
        Nothing -> let l k = fromJust . lookup k . credsExtra $ creds in
                   Just <$> insert User { userEmail = l "email"
                                        , userLogin = l "login"
                                        , userAvatarUrl = l "avatar_url"
                                        , userAccessToken = l "access_token"
                                        , userName = l "name"}

    loginDest _ = HomeR
    logoutDest _ = HomeR

    authPlugins _ = [ oauth2GithubScoped clientId secret scopes ]
      where clientId = "b0390cb88607d80378d7"
            secret = "6a42c43203623ec454d75d4a8639ff18ca4baf0a"
            scopes = ["user:email", "repo", "write:repo_hook"]

    authHttpManager = httpManager

instance YesodAuthPersist Approvd

instance RenderMessage Approvd FormMessage where
    renderMessage _ _ = defaultFormMessage


getPayloadUrl :: Handler Text
getPayloadUrl = do render <- getUrlRender
                   return $ render PayloadR


getHomeR :: Handler Html
getHomeR = do mauth <- maybeAuth
              defaultLayout $ do setTitle "Welcome to Approvd"
                                 $(whamletFile "templates/index.hamlet")

postPayloadR :: Handler ()
postPayloadR = do eventType <- lookupHeader "X-Github-Event"
                  when (eventType == Just "issue_comment") $ do
                    -- TODO: check it is indeed a PR
                    value <- requireJsonBody :: Handler Value
                    let prId = truncate $ fromJust $ value ^? key "issue" . key "number" . _Number
                        repo = undefined
                    token <- undefined -- TODO: Damn, I need a database
                    liftIO $ runGithub token $ handlePR repo prId

getRepoR :: Handler Html
getRepoR = do Entity _ auth <- requireAuth
              let token = encodeUtf8 $ userAccessToken auth
              payloadUrl <- getPayloadUrl
              repos <- liftIO $ runGithub token $ getRepos payloadUrl
              defaultLayout $(whamletFile "templates/repos.hamlet")

postRepoR :: Handler ()
postRepoR = undefined
