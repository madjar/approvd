{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Approvd.Github (Github, runGithub, get, post) where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List (intercalate)
import qualified Network.Wreq as W
import Network.Wreq.Types (Postable)

data GithubConfig = GithubConfig { gToken :: B.ByteString }

-- | A monad that call the github api with a given authentication
newtype Github a = Github { runG :: ReaderT GithubConfig IO a }
                 deriving (Functor, Monad, Applicative)

-- | Run Github with a given auth and extract the final value
runGithub :: B.ByteString  -- ^ Authentication token
          -> Github a      -- ^ The Github action to run
          -> IO a
runGithub token g = runReaderT (runG g) (GithubConfig token)

-- | Run a get request to a github endpoint in the Github monad
get :: [String] -> Github L.ByteString
get url = do opts <- options
             r <- Github . liftIO $ W.getWith opts (buildUrl url)
             return $ r ^. W.responseBody

-- | Run a post request to a github endpoint in the Github monad
post :: Postable a => [String] -> a -> Github L.ByteString
post url p = do opts <- options
                r <- Github . liftIO $ W.postWith opts (buildUrl url) p
                return $ r ^. W.responseBody

-- | Github's API base url
githubUrl :: String
githubUrl = "https://api.github.com"

-- | Build a github api url from a list of components
buildUrl :: [String] -> String
buildUrl components = intercalate "/" (githubUrl:components)

-- | The user agent of our bot
userAgent :: B.ByteString
userAgent = "Approvd.io bot/0.1 (@madjar)"

-- | Returns the wreq options in the Github monad
options :: Github W.Options
options = do config <- Github ask
             return $ W.defaults & W.header "User-Agent" .~ [userAgent]
                                 & W.auth ?~ W.oauth2Token (gToken config)
