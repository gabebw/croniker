module Model.Profile
    ( todayAndFutureProfilesFor
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
    { profileMoniker :: Maybe Text
    , profileDate :: Day
    , profilePicture :: Maybe FileInfo
    }

addProfile :: UserId -> FormProfile -> Handler ()
addProfile userId (FormProfile moniker date picture) = do
    base64Picture <- base64Bytes picture
    void $ runDB $ insert $ Profile moniker date userId base64Picture False

-- If a picture was uploaded, base64-encode it.
base64Bytes :: Maybe FileInfo -> Handler (Maybe Text)
base64Bytes Nothing = return Nothing
base64Bytes (Just fi) = do
    fileBytes <- runResourceT $ fileSource fi $$ sinkLbs
    return $ Just $ base64Encode fileBytes

findProfileFor :: UserId -> ProfileId -> DB (Maybe (Entity Profile))
findProfileFor userId profileId = selectFirst [ProfileUserId ==. userId, ProfileId ==. profileId] []

todayAndFutureProfilesFor :: Entity User -> DB [Entity Profile]
todayAndFutureProfilesFor (Entity userId user) = do
    day <- localToday user
    selectList [ProfileUserId ==. userId, ProfileDate >=. day] [Asc ProfileDate]

-- Due to time zones, we need to find profiles from yesterday/today/tomorrow (in
-- UTC) because "yesterday"/"tomorrow" in UTC may be "today" in a user's time
-- zone.
allProfilesForUpdate :: DB [Entity Profile]
allProfilesForUpdate = do
    days <- sequence [yesterday, today, tomorrow]
    selectList [ProfileDate <-. days, ProfileSent ==. False] [Asc ProfileDate]
