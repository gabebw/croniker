module Croniker.Time
    ( today
    , tomorrow
    , yesterday
    , localTime
    ) where

import ClassyPrelude.Yesod
import Model
import Data.Time
import Data.Time.Zones (timeZoneForUTCTime)
import Data.Time.Zones.All (tzByLabel)

today :: (MonadIO m) => m Day
today = liftIO getCurrentTime >>= dayM

tomorrow :: (MonadIO m) => m Day
tomorrow = liftIO getCurrentTime >>= dayM . addUTCTime oneDay

yesterday :: (MonadIO m) => m Day
yesterday = liftIO getCurrentTime >>= dayM . addUTCTime (-1 * oneDay)

localTime :: UTCTime -> User -> LocalTime
localTime utcNow user = utcToLocalTime timezone utcNow
    where
        timezone = timeZoneForUTCTime tz utcNow
        tz = tzByLabel $ userTzLabel user

dayM :: (MonadIO m) => UTCTime -> m Day
dayM = return . utctDay

oneDay :: NominalDiffTime
oneDay = 60 * 60 * 24
