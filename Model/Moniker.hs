module Model.Moniker
    ( futureMonikersFor
    , monikersFromTodayFor
    , allMonikersForUpdate
    , findMonikerFor
    ) where

import Import

import Data.RelativeTimes

findMonikerFor :: UserId -> MonikerId -> DB (Maybe (Entity Moniker))
findMonikerFor userId monikerId = selectFirst [MonikerUserId ==. userId, MonikerId ==. monikerId] []

futureMonikersFor :: UserId -> DB [Entity Moniker]
futureMonikersFor userId = do
    day <- today
    selectList [MonikerUserId ==. userId, MonikerDate >=. day] [Asc MonikerDate]

monikersFromTodayFor :: UserId -> DB [Entity Moniker]
monikersFromTodayFor userId = do
    day <- today
    selectList [MonikerUserId ==. userId, MonikerDate ==. day] [Asc MonikerDate]

-- Due to time zones, we need to find monikers from yesterday/today/tomorrow (in
-- UTC) because "yesterday"/"tomorrow" in UTC may be "today" in a user's time
-- zone.
allMonikersForUpdate :: DB [Entity Moniker]
allMonikersForUpdate = do
    days <- sequence [yesterday, today, tomorrow]
    selectList [MonikerDate <-. days] [Asc MonikerDate]
