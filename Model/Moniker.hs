module Model.Moniker
    ( futureMonikersFor
    , monikersFromTodayFor
    , allMonikersFromToday
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

allMonikersFromToday :: DB [Entity Moniker]
allMonikersFromToday = do
    day <- today
    selectList [MonikerDate ==. day] [Asc MonikerDate]
