module Model.Moniker
    ( allMonikers
    , monikersFromToday
    , today
    , tomorrow
    ) where

import Import

import Data.Time.Clock (addUTCTime)

allMonikers :: DB [Entity Moniker]
allMonikers = selectList [] [Asc MonikerDate]

monikersFromToday :: DB [Entity Moniker]
monikersFromToday = (liftIO today) >>= monikersFrom

monikersFrom :: Day -> DB [Entity Moniker]
monikersFrom day = selectList [MonikerDate ==. day] [Asc MonikerDate]

today :: IO Day
today = getCurrentTime >>= return . utctDay

tomorrow :: IO Day
tomorrow = do
    let oneDay = fromInteger $ 60 * 60 * 24
    getCurrentTime >>= return . utctDay . (addUTCTime oneDay)
