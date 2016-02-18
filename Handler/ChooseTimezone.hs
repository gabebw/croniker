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
        $(widgetFile "choose_timezone")
