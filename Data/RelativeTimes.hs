module Data.RelativeTimes
    ( today
    , tomorrow
    ) where

import ClassyPrelude.Yesod

import Data.Time

today :: (MonadIO m) => m Day
today = liftIO getCurrentTime >>= dayM

tomorrow :: (MonadIO m) => m Day
tomorrow = liftIO getCurrentTime >>= dayM . addUTCTime oneDay

dayM :: (MonadIO m) => UTCTime -> m Day
dayM = return . utctDay

oneDay :: NominalDiffTime
oneDay = 60 * 60 * 24
