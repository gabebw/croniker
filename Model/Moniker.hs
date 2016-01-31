module Model.Moniker
    ( allMonikers
    , monikersFromToday
    , today
    ) where

import Import

allMonikers :: DB [Entity Moniker]
allMonikers = selectList [] [Asc MonikerDate]

monikersFromToday :: DB [Entity Moniker]
monikersFromToday = (liftIO today) >>= monikersFrom

monikersFrom :: Day -> DB [Entity Moniker]
monikersFrom day = selectList [MonikerDate ==. day] [Asc MonikerDate]

today :: IO Day
today = getCurrentTime >>= return . utctDay
