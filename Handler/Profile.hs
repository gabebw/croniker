{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Profile
    ( getProfileR
    , postProfileR
    , postDeleteProfileR
    ) where

import Import

import Data.Maybe (fromJust)
import Data.Time.Format (FormatTime)
import Text.Blaze (ToMarkup, toMarkup)

import Helper.Request (fromMaybe404)
import qualified Croniker.MonikerNormalization as CMN
import qualified Croniker.Time as CT
import qualified Croniker.UrlParser as CUP
import qualified Model.Profile as P
import qualified Model.User as U

instance ToMarkup Day where
  toMarkup = toMarkup . show

getProfileR :: Handler Html
getProfileR = do
    euser@(Entity userId user) <- prerequisites
    tomorrow <- CT.localTomorrow user
    takenDays <- runDB $ U.takenDays tomorrow userId
    nextFreeDay <- runDB $ U.nextFreeDay tomorrow takenDays
    widget <- fst <$> (generateFormPost $ profileForm nextFreeDay takenDays tomorrow)
    profilesTemplate euser widget

postProfileR :: Handler Html
postProfileR = do
    euser@(Entity userId user) <- prerequisites
    tomorrow <- CT.localTomorrow user
    takenDays <- runDB $ U.takenDays tomorrow userId
    nextFreeDay <- runDB $ U.nextFreeDay tomorrow takenDays
    (result, formWidget) <- fst <$> (runFormPost $ profileForm nextFreeDay takenDays tomorrow)

    case result of
        FormSuccess formProfile  -> do
            P.addProfile userId formProfile
            setMessage "Profile created"
            redirect ProfileR
        _ -> do
            setMessage "Oops, something went wrong"
            profilesTemplate euser formWidget

postDeleteProfileR :: ProfileId -> Handler ()
postDeleteProfileR profileId = do
    requireOwnedProfile profileId
    runDB $ delete profileId
    setMessage "Profile deleted!"
    redirect ProfileR

prerequisites :: Handler (Entity User)
prerequisites = do
    euser@(Entity _ user) <- requireAuth
    requireSetTimezone user
    return euser

requireSetTimezone :: User -> Handler ()
requireSetTimezone user = unless (userChoseTimezone user) (redirect ChooseTimezoneR)

profilesTemplate :: (ToWidget App w) => Entity User -> w -> Handler Html
profilesTemplate (Entity userId _) profileWidget = do
    csrfToken <- fromJust . reqToken <$> getRequest
    allProfiles <- runDB $ P.futureProfilesFor userId
    defaultLayout $ do
        setTitle "Croniker"
        $(widgetFile "profiles")

requireOwnedProfile :: ProfileId -> Handler ()
requireOwnedProfile profileId = do
    userId <- requireAuthId
    void $ fromMaybe404 $ runDB $ P.findProfileFor userId profileId

profileForm :: Day -> [Day] -> Day -> Form P.FormProfile
profileForm nextFreeDay takenDays tomorrow = renderDivs $ P.FormProfile
    <$> fmap CMN.normalize (areq nameField (fs "New moniker" [("maxlength", "20"), ("autofocus", "autofocus")]) Nothing)
    <*> areq
            (dateField takenDays tomorrow)
            ((fs "Date" []) { fsTooltip = Just "Defaults to the next available date" })
            (Just nextFreeDay)
    <*> aopt fileField (fs "Profile picture (optional)" []) Nothing

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

nameField :: Field Handler Text
nameField = foldr check textField [
                doesNotContainUrl,
                doesNotContainTwitter,
                validWhitespace . CMN.normalize,
                validLength . CMN.normalize
            ]

doesNotContainTwitter :: Text -> Either Text Text
doesNotContainTwitter name
    | "twitter" `isInfixOf` toLower name = Left "Twitter doesn't allow monikers that contain \"Twitter\""
    | otherwise = Right name

doesNotContainUrl :: Text -> Either Text Text
doesNotContainUrl name
    | CUP.containsUrl name = Left "Twitter doesn't allow URLs in monikers"
    | otherwise = Right name

validLength :: Text -> Either Text Text
validLength name
    | length name == 0 = Left "Usernames cannot be blank"
    | length name > 20 = Left "Twitter doesn't allow profiles longer than 20 characters"
    | otherwise = Right name

validWhitespace :: Text -> Either Text Text
validWhitespace name
    | any (`isInfixOf` name) ["\n", "\t"] = Left "Usernames cannot contain special whitespace characters"
    | otherwise = Right name

dateField :: [Day] -> Day -> Field Handler Day
dateField takenDays tomorrow = check (nothingScheduled takenDays) $ check (futureDate tomorrow) dayField

futureDate :: Day -> Day -> Either Text Day
futureDate tomorrow date
    | date < tomorrow = Left "You must select a future date"
    | otherwise = Right date

nothingScheduled :: [Day] -> Day -> Either Text Day
nothingScheduled takenDays date
    | date `elem` takenDays = Left "You already have a change scheduled for that day"
    | otherwise = Right date
