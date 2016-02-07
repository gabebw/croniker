module TwitterClient (
    updateTwitterName
    ) where

import Import
import Control.Lens ((.~), (&), (?~))
import Network.Wreq as Wreq (auth, defaults, postWith, param, oauth1Auth, Auth)
import System.Environment (getEnv)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

-- https://dev.twitter.com/rest/reference/post/account/update_profile
updateTwitterName :: T.Text -> IO ()
updateTwitterName newName = do
    let url = "https://api.twitter.com/1.1/account/update_profile.json"
    twitterAuth <- twitterAuthentication
    let opts = defaults & param "name" .~ [newName] & auth ?~ twitterAuth
    -- Null because we don't send anything in the post body
    void $ postWith opts url Null

twitterAuthentication :: IO Wreq.Auth
twitterAuthentication = do
    consumerKey <- BSC.pack <$> getEnv "TWITTER_CONSUMER_KEY"
    consumerSecret <- BSC.pack <$> getEnv "TWITTER_CONSUMER_SECRET"
    accessKey <- BSC.pack <$> getEnv "TWITTER_ACCESS_KEY"
    accessSecret <- BSC.pack <$> getEnv "TWITTER_ACCESS_SECRET"
    return $ oauth1Auth consumerKey consumerSecret accessKey accessSecret
