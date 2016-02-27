module Handler.ChooseTimezone
    ( getChooseTimezoneR
    ) where

import Import

import Handler.UpdateUser (timezoneForm)

getChooseTimezoneR :: Handler Html
getChooseTimezoneR = do
    (Entity userId user) <- requireAuth
    (tzWidget, tzEnc) <- generateFormPost (timezoneForm user)
    defaultLayout $ do
        setTitle "Croniker"
        addScript $ StaticR javascript_selectTimezone_js
        $(widgetFile "choose_timezone")
