{-# LANGUAGE NamedFieldPuns #-}

module Handler.TodaysMonikersTask
    ( updateTodaysMonikers
    ) where

import Import

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Model.Moniker (allMonikersForUpdate)
import qualified Croniker.Time as CT
import TwitterClient (updateTwitterName)

updateTodaysMonikers :: Handler ()
updateTodaysMonikers = do
    monikers <- map entityVal <$> runDB allMonikersForUpdate
    filteredMonikers <- filterM isTime monikers
    mapM_ updateName filteredMonikers

updateName :: Moniker -> Handler ()
updateName (Moniker name _ userId) = do
    App{twitterConsumerKey, twitterConsumerSecret} <- getYesod
    muser <- runDB $ get userId
    case muser of
        Nothing -> return ()
        (Just user) -> do
            let accessKey = BSC.pack $ T.unpack $ userTwitterOauthToken user
            let accessSecret = BSC.pack $ T.unpack $ userTwitterOauthTokenSecret user
            liftIO $ updateTwitterName name twitterConsumerKey twitterConsumerSecret accessKey accessSecret

isTime :: Moniker -> Handler Bool
isTime (Moniker _ monikerDay userId) = do
    muser <- runDB $ get userId
    case muser of
      Nothing -> return False
      (Just user) -> (monikerDay ==) <$> CT.localToday user
