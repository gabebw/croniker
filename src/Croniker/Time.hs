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
today = todayPlus 0

tomorrow :: (MonadIO m) => m Day
tomorrow = todayPlus 1

yesterday :: (MonadIO m) => m Day
yesterday = todayPlus (-1)

localTime :: UTCTime -> User -> LocalTime
localTime utcNow user = utcToLocalTime timezone utcNow
    where
        timezone = timeZoneForUTCTime tz utcNow
        tz = tzByLabel $ userTzLabel user

todayPlus :: (MonadIO m) => NominalDiffTime -> m Day
todayPlus diff = liftIO getCurrentTime >>= dayM . addUTCTime (oneDay * diff)
    where
        dayM :: (MonadIO m) => UTCTime -> m Day
        dayM = return . utctDay

        oneDay :: NominalDiffTime
        oneDay = 60 * 60 * 24
