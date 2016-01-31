module Handler.TodaysMonikersTask where

import Import
import Model.Moniker

printTodaysMonikers :: Handler ()
printTodaysMonikers = do
    todaysMonikers <- runDB monikersFromToday
    print $ map entityVal todaysMonikers
