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
updateTwitterName :: T.Text -> ByteString -> ByteString -> ByteString -> ByteString ->  IO ()
updateTwitterName newName consumerKey consumerSecret accessKey accessSecret = do
    let url = "https://api.twitter.com/1.1/account/update_profile.json"
    let twitterAuth = oauth1Auth consumerKey consumerSecret accessKey accessSecret
    let opts = defaults & param "name" .~ [newName] & auth ?~ twitterAuth
    -- Null because we don't send anything in the post body
    void $ postWith opts url Null
