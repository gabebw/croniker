module TwitterClient
    ( updateTwitterMoniker
    , updateTwitterPicture
    , updateTwitterDescription
    ) where

import Import

import Control.Lens ((.~), (&), (?~))
import Network.Wreq (FormParam((:=)), auth, defaults, postWith, param)

import Croniker.Types (OauthReader)

updateTwitterMoniker :: Text -> OauthReader ()
updateTwitterMoniker = updateProfile "name"

updateTwitterDescription :: Text -> OauthReader ()
updateTwitterDescription = updateProfile "description"

-- https://dev.twitter.com/rest/reference/post/account/update_profile
updateProfile :: Text -> Text -> OauthReader ()
updateProfile name value = do
    oauth <- ask
    let url = "https://api.twitter.com/1.1/account/update_profile.json"
    let opts = defaults & param name .~ [value] & auth ?~ oauth
    void $ liftIO $ postWith opts url Null

-- https://dev.twitter.com/rest/reference/post/account/update_profile_image
updateTwitterPicture :: Text -> OauthReader ()
updateTwitterPicture b64image = do
    oauth <- ask
    let url = "https://api.twitter.com/1.1/account/update_profile_image.json"
    let opts = defaults & auth ?~ oauth
    void $ liftIO $ postWith opts url ["image" := b64image]
