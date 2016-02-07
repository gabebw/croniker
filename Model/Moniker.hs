module Model.Moniker
    ( futureMonikersFor
    , monikersFromTodayFor
    , allMonikersFromToday
    , today
    , tomorrow
    ) where

import Import

import Data.Time.Clock (addUTCTime)

futureMonikersFor :: UserId -> DB [Entity Moniker]
futureMonikersFor userId = do
    day <- liftIO today
    selectList [MonikerUserId ==. userId, MonikerDate >=. day] [Asc MonikerDate]

monikersFromTodayFor :: UserId -> DB [Entity Moniker]
monikersFromTodayFor userId = do
    day <- liftIO today
    selectList [MonikerUserId ==. userId, MonikerDate ==. day] [Asc MonikerDate]

allMonikersFromToday :: DB [Entity Moniker]
allMonikersFromToday = do
    day <- liftIO today
    selectList [MonikerDate ==. day] [Asc MonikerDate]

today :: IO Day
today = getCurrentTime >>= return . utctDay

tomorrow :: IO Day
tomorrow = do
    let oneDay = fromInteger $ 60 * 60 * 24
    getCurrentTime >>= return . utctDay . (addUTCTime oneDay)
