module Handler.TodaysMonikersTask where

import Import
import Model.Moniker (monikersFromToday)
import TwitterClient (updateTwitterName)

updateTodaysMonikers :: Handler ()
updateTodaysMonikers = do
    monikers <- runDB monikersFromToday
    let monikerNames = map (monikerName . entityVal) monikers
    liftIO $ mapM_ updateTwitterName monikerNames
