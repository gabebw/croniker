module Model.Profile
    ( todayAndFutureProfilesFor
    , allProfilesForUpdate
    , findProfileFor
    ) where

import Import

import Croniker.Time

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
