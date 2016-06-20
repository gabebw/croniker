module Handler.UpdateUser
    ( postUpdateUserR
    , timezoneForm
    ) where

import Import

import Data.Time.Zones.All (TZLabel(..), toTZName)
import Helper.TextConversion

postUpdateUserR :: UserId -> Handler Html
postUpdateUserR userId = do
    (Entity _ user) <- requireAuth
    ((result, _), _) <- runFormPost (timezoneForm user)
    case result of
        FormSuccess newUser -> do
            void $ runDB $ replace userId $ newUser { userChoseTimezone = True }
            redirect RootR
        _ -> do
            setMessage "Oops, something went wrong"
            redirect RootR

timezoneForm :: User -> Form User
timezoneForm user = renderDivs $ setTimezone
    <$> areq (selectFieldList selectTZs) "" Nothing
    where
        selectTZs :: [(Text, TZLabel)]
        selectTZs = map (\tzlabel -> (b2t $ toTZName tzlabel, tzlabel)) [Africa__Abidjan .. Root__WET]

        setTimezone tzLabel = user { userTzLabel = tzLabel }
