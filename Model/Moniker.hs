module Model.Moniker
    ( allMonikers
    , monikersFrom
    ) where

import Import

allMonikers :: DB [Entity Moniker]
allMonikers = selectList [] [Asc MonikerDate]

monikersFrom :: Day -> DB [Entity Moniker]
monikersFrom day = selectList [MonikerDate ==. day] [Asc MonikerDate]
