module TwitterClient
    ( updateTwitterName
    , updateTwitterPicture
    ) where

import Import

import Control.Lens ((.~), (&), (?~))
import Network.Wreq (FormParam((:=)), auth, defaults, postWith, param, oauth1Auth)

-- https://dev.twitter.com/rest/reference/post/account/update_profile
updateTwitterName :: Text -> ByteString -> ByteString -> ByteString -> ByteString -> IO ()
updateTwitterName newName consumerKey consumerSecret accessKey accessSecret = do
    let url = "https://api.twitter.com/1.1/account/update_profile.json"
    let twitterAuth = oauth1Auth consumerKey consumerSecret accessKey accessSecret
    let opts = defaults & param "name" .~ [newName] & auth ?~ twitterAuth
    -- Null because we don't send anything in the post body
    void $ postWith opts url Null

-- https://dev.twitter.com/rest/reference/post/account/update_profile_image
updateTwitterPicture :: Text -> ByteString -> ByteString -> ByteString -> ByteString -> IO ()
updateTwitterPicture b64image consumerKey consumerSecret accessKey accessSecret = do
    let url = "https://api.twitter.com/1.1/account/update_profile_image.json"
    let twitterAuth = oauth1Auth consumerKey consumerSecret accessKey accessSecret
    let opts = defaults & auth ?~ twitterAuth
    void $ postWith opts url ["image" := b64image]
