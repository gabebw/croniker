module Croniker.Time
    ( today
    , tomorrow
    , yesterday
    ) where

import ClassyPrelude.Yesod

import Data.Time

today :: (MonadIO m) => m Day
today = liftIO getCurrentTime >>= dayM

tomorrow :: (MonadIO m) => m Day
tomorrow = liftIO getCurrentTime >>= dayM . addUTCTime oneDay

yesterday :: (MonadIO m) => m Day
yesterday = liftIO getCurrentTime >>= dayM . addUTCTime (-1 * oneDay)

dayM :: (MonadIO m) => UTCTime -> m Day
dayM = return . utctDay

oneDay :: NominalDiffTime
oneDay = 60 * 60 * 24
