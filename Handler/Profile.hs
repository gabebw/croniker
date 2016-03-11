{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Profile
    ( getProfileR
    , postDeleteProfileR
    , postProfileR
    ) where

import Import

import Data.Maybe (fromJust)
import Data.Time.Format (FormatTime)
import Text.Blaze (ToMarkup, toMarkup)
import Helper.Request (fromMaybe404)
import qualified Model.Profile as M
import qualified Croniker.Time as CT

instance ToMarkup Day where
  toMarkup = toMarkup . show

prerequisites :: Handler (Entity User)
prerequisites = do
    euser@(Entity _ user) <- requireAuth
    requireSetTimezone user
    return euser

getProfileR :: Handler Html
getProfileR = do
    euser@(Entity userId user) <- prerequisites
    tomorrow <- CT.localTomorrow user
    profileFormPost <- generateFormPost $ profileForm tomorrow userId
    profilesTemplate euser profileFormPost

requireSetTimezone :: User -> Handler ()
requireSetTimezone user = when (not $ userChoseTimezone user) (redirect ChooseTimezoneR)

postProfileR :: Handler Html
postProfileR = do
    euser@(Entity userId user) <- prerequisites
    tomorrow <- CT.localTomorrow user
    ((result, formWidget), formEnctype) <- runFormPost $ profileForm tomorrow userId
    case result of
        FormSuccess profile -> do
            void $ runDB $ insert profile
            setMessage "Profile created"
            redirect ProfileR
        _ -> do
            setMessage "Oops, something went wrong"
            profilesTemplate euser (formWidget, formEnctype)

profilesTemplate :: (ToWidget App w) => Entity User -> (w, Enctype) -> Handler Html
profilesTemplate (Entity userId _) (profileWidget, profileEnc) = do
    csrfToken <- fromJust . reqToken <$> getRequest
    allProfiles <- runDB $ M.futureProfilesFor userId
    defaultLayout $ do
        setTitle "Croniker"
        $(widgetFile "profiles")

postDeleteProfileR :: ProfileId -> Handler ()
postDeleteProfileR profileId = do
    requireOwnedProfile profileId
    runDB $ delete profileId
    setMessage "Profile deleted!"
    redirect ProfileR

requireOwnedProfile :: ProfileId -> Handler ()
requireOwnedProfile profileId = do
    userId <- requireAuthId
    void $ fromMaybe404 $ runDB $ M.findProfileFor userId profileId

profileForm :: Day -> UserId -> Form Profile
profileForm tomorrow userId = renderDivs $ Profile
       <$> areq nameField (fs "New profile" [("maxlength", "20")]) Nothing
       <*> areq dateField (fs "Date" []) (Just tomorrow)
       <*> pure userId
    where
        dateField = check futureDate dayField
        nameField = check maxLength textField
        futureDate :: Day -> Either Text Day
        futureDate date
            | date < tomorrow = Left "You must select a future date"
            | otherwise = Right date
        maxLength :: Text -> Either Text Text
        maxLength name
            | length name > 20 = Left "Twitter doesn't allow profiles longer than 20 characters"
            | otherwise = Right name
        fs :: Text -> [(Text, Text)] -> FieldSettings site
        fs label attrs = FieldSettings
            { fsLabel = SomeMessage label
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = attrs
            }

prettyTime :: (FormatTime t) => t -> String
prettyTime = formatTime defaultTimeLocale "%B %d, %Y"
