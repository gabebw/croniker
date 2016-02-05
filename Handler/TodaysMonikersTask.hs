module Handler.TodaysMonikersTask where

import Import
import Model.Moniker (allMonikersFromToday)
import TwitterClient (updateTwitterName)

updateTodaysMonikers :: Handler ()
updateTodaysMonikers = do
    monikers <- runDB allMonikersFromToday
    let monikerNames = map (monikerName . entityVal) monikers
    liftIO $ mapM_ updateTwitterName monikerNames
