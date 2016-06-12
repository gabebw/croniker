module Model.Profile
    ( futureProfilesFor
    , allProfilesForUpdate
    , findProfileFor
    , addProfile
    , FormProfile(..)
    ) where

import Import

import Croniker.Time
import Data.Conduit.Binary (sinkLbs)
import Helper.TextConversion (base64Encode)

data FormProfile = FormProfile
    { profileName :: Text
    , profileDate :: Day
    , profileUserId :: UserId
    , profilePicture :: Maybe FileInfo
    , profileSent :: Bool
    }

addProfile :: FormProfile -> Handler ()
addProfile (FormProfile name date userId picture sent) = do
    base64Picture <- base64Bytes picture
    void $ runDB $ insert $ Profile name date userId base64Picture sent

-- If a picture was uploaded, base64-encode it.
base64Bytes :: Maybe FileInfo -> Handler (Maybe Text)
base64Bytes Nothing = return Nothing
base64Bytes (Just fi) = do
    fileBytes <- runResourceT $ fileSource fi $$ sinkLbs
    return $ Just $ base64Encode fileBytes

findProfileFor :: UserId -> ProfileId -> DB (Maybe (Entity Profile))
findProfileFor userId profileId = selectFirst [ProfileUserId ==. userId, ProfileId ==. profileId] []

futureProfilesFor :: UserId -> DB [Entity Profile]
futureProfilesFor userId = do
    day <- today
    selectList [ProfileUserId ==. userId, ProfileDate >=. day] [Asc ProfileDate]

-- Due to time zones, we need to find profiles from yesterday/today/tomorrow (in
-- UTC) because "yesterday"/"tomorrow" in UTC may be "today" in a user's time
-- zone.
allProfilesForUpdate :: DB [Entity Profile]
allProfilesForUpdate = do
    days <- sequence [yesterday, today, tomorrow]
    selectList [ProfileDate <-. days, ProfileSent ==. False] [Asc ProfileDate]
