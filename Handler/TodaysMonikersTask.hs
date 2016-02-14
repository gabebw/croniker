module Handler.TodaysMonikersTask
    ( updateTodaysMonikers
    ) where

import Import

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Time.Zones (timeZoneForUTCTime)
import Data.Time.Zones.All (tzByLabel)
import Data.Time.LocalTime (utcToLocalTime, LocalTime(..))
import Model.Moniker (allMonikersForUpdate)
import TwitterClient (updateTwitterName)

updateTodaysMonikers :: Handler ()
updateTodaysMonikers = do
    App{..} <- getYesod
    now <- liftIO $ getCurrentTime
    monikers <- map entityVal <$> runDB allMonikersForUpdate
    filteredMonikers <- filterM (isTime now) monikers
    mapM_ (updateName twitterConsumerKey twitterConsumerSecret) filteredMonikers

updateName :: ByteString -> ByteString -> Moniker -> Handler ()
updateName consumerKey consumerSecret (Moniker name _ userId) = do
    muser <- runDB $ get userId
    case muser of
        Nothing -> return ()
        (Just user) -> do
            let accessKey = BSC.pack $ T.unpack $ userTwitterOauthToken user
            let accessSecret = BSC.pack $ T.unpack $ userTwitterOauthTokenSecret user
            liftIO $ updateTwitterName name consumerKey consumerSecret accessKey accessSecret

isTime :: UTCTime -> Moniker -> Handler Bool
isTime utcNow (Moniker _ monikerDay userId) = do
    muser <- runDB $ get userId
    return $ case muser of
      Nothing -> False
      (Just user) -> (userDay utcNow user) == monikerDay

-- Convert the current time in UTC into the user's current day.
-- For example, if it's February 22nd in UTC, it may be February 21 in PST.
-- This task runs ~every hour, so if someone sets their moniker for today at 8pm,
-- it'll get set at (roughly) 9pm that day.
-- If they set it for tomorrow, it'll get set at (roughly) 12am.
userDay :: UTCTime -> User -> Day
userDay utcNow user = localDay $ utcToLocalTime timezone utcNow
    where
        timezone = timeZoneForUTCTime tz utcNow
        tz = tzByLabel $ userTzLabel user
