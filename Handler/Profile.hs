{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Profile
    ( getProfileR
    , postProfileR
    , postDeleteProfileR
    ) where

import Import

import Data.Conduit.Binary (sinkLbs)
import Data.Maybe (fromJust)
import Data.Time.Format (FormatTime)
import Text.Blaze (ToMarkup, toMarkup)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as L

import Helper.Request (fromMaybe404)
import Helper.TextConversion (b2t)
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
    takenDays <- runDB $ U.takenDays userId
    (widget, _) <- generateFormPost $ profileForm takenDays tomorrow userId
    profilesTemplate euser widget

postProfileR :: Handler Html
postProfileR = do
    euser@(Entity userId user) <- prerequisites
    tomorrow <- CT.localTomorrow user
    takenDays <- runDB $ U.takenDays userId
    ((resultWithoutProfilePicture, formWidget), _) <- runFormPost $ profileForm takenDays tomorrow userId
    result <- withPossibleProfilePicture resultWithoutProfilePicture

    case result of
        FormSuccess profile -> do
            void $ runDB $ insert profile
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

fileContents :: (Text, FileInfo) -> Handler L.ByteString
fileContents (_, fi) = runResourceT $ fileSource fi $$ sinkLbs

-- If there's a profile picture in the HTTP request, set the Profile's
-- profilePicture field to the Base64-encoded contents of that file.
--
-- If there's no profile picture, don't do anything.
withPossibleProfilePicture :: FormResult Profile -> Handler (FormResult Profile)
withPossibleProfilePicture (FormSuccess profile) = do
    (_, files) <- runRequestBody
    contents <- mapM fileContents files
    return $ FormSuccess $ profileWithPicture contents profile
    where
        profileWithPicture (f:_) p = p { profilePicture = Just $ base64text f }
        profileWithPicture _ p = p
        base64text = b2t . B64.encode . toStrict

withPossibleProfilePicture result = return result

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

profileForm :: [Day] -> Day -> UserId -> Form Profile
profileForm takenDays tomorrow userId = renderDivs $ Profile
    <$> fmap CMN.normalize (areq nameField (fs "New profile" [("maxlength", "20"), ("autofocus", "autofocus")]) Nothing)
    <*> areq (dateField takenDays tomorrow) (fs "Date" []) (Just tomorrow)
    <*> pure userId
    <*> (Nothing <$ aopt fileField (fs "Profile picture (optional)" []) Nothing)
    <*> pure False

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
