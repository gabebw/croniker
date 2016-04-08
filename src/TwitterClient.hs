module TwitterClient
    ( updateTwitterName
    , updateTwitterPicture
    ) where

import Import

import Control.Lens ((.~), (&), (?~))
import Network.Wreq (FormParam((:=)), auth, defaults, postWith, param)
import Croniker.Types (OauthCredentials, toOauth1Auth)

-- https://dev.twitter.com/rest/reference/post/account/update_profile
updateTwitterName :: Text -> OauthCredentials -> IO ()
updateTwitterName newName oauthCredentials = do
    let url = "https://api.twitter.com/1.1/account/update_profile.json"
    let opts = defaults & param "name" .~ [newName] & auth ?~ (toOauth1Auth oauthCredentials)
    -- Null because we don't send anything in the post body
    void $ postWith opts url Null

-- https://dev.twitter.com/rest/reference/post/account/update_profile_image
updateTwitterPicture :: Text -> OauthCredentials -> IO ()
updateTwitterPicture b64image oauthCredentials = do
    let url = "https://api.twitter.com/1.1/account/update_profile_image.json"
    let opts = defaults & auth ?~ (toOauth1Auth oauthCredentials)
    void $ postWith opts url ["image" := b64image]
