module Croniker.Time
    ( today
    , tomorrow
    , yesterday
    , localToday
    , localTomorrow
    ) where

import ClassyPrelude.Yesod
import Model (User(..))
import Data.Time
import Data.Time.Zones (timeZoneForUTCTime)
import Data.Time.Zones.All (tzByLabel)

today :: (MonadIO m) => m Day
today = todayPlus 0

tomorrow :: (MonadIO m) => m Day
tomorrow = todayPlus 1

yesterday :: (MonadIO m) => m Day
yesterday = todayPlus (-1)

-- Convert the current time in UTC into the user's current day.
-- For example, if it's February 22nd in UTC, it may be February 21 in PST.
localToday :: (MonadIO m) => User -> m Day
localToday user = localDay <$> localTime id user

localTomorrow :: (MonadIO m) => User -> m Day
localTomorrow user = localDay <$> localTime (addUTCTime oneDay) user

-- Apply a function to the current time in UTC and return the transformed
-- UTCTime as a LocalTime in the given user's timezone.
localTime :: (MonadIO m) => (UTCTime -> UTCTime) -> User -> m LocalTime
localTime f user = do
    now <- f <$> liftIO getCurrentTime
    return $ utcToLocalTime (timezone now) now
    where
        timezone = timeZoneForUTCTime tz
        tz = tzByLabel $ userTzLabel user

todayPlus :: (MonadIO m) => NominalDiffTime -> m Day
todayPlus diff = liftIO getCurrentTime >>= dayM . addUTCTime (oneDay * diff)
    where
        dayM :: (MonadIO m) => UTCTime -> m Day
        dayM = return . utctDay

oneDay :: NominalDiffTime
oneDay = 60 * 60 * 24
