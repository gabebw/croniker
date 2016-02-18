module Handler.TodaysMonikersTask
    ( updateTodaysMonikers
    ) where

import Import

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Model.Moniker (allMonikersFromToday)
import TwitterClient (updateTwitterName)

updateTodaysMonikers :: Handler ()
updateTodaysMonikers = do
    App{..} <- getYesod
    monikers <- map entityVal <$> runDB allMonikersFromToday
    mapM_ (updateName twitterConsumerKey twitterConsumerSecret) monikers
    where
        updateName :: ByteString -> ByteString -> Moniker -> Handler ()
        updateName consumerKey consumerSecret (Moniker name _ userId) = do
            muser <- runDB $ get userId
            case muser of
                Nothing -> return ()
                (Just user) -> do
                    let accessKey = BSC.pack $ T.unpack $ userTwitterOauthToken user
                    let accessSecret = BSC.pack $ T.unpack $ userTwitterOauthTokenSecret user
                    liftIO $ updateTwitterName name consumerKey consumerSecret accessKey accessSecret
