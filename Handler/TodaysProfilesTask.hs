{-# LANGUAGE NamedFieldPuns #-}

module Handler.TodaysProfilesTask
    ( updateTodaysProfiles
    ) where

import Import

import Model.Profile (allProfilesForUpdate)
import qualified Croniker.Time as CT
import TwitterClient (updateTwitterName)
import Helper.TextConversion

updateTodaysProfiles :: Handler ()
updateTodaysProfiles = do
    profiles <- map entityVal <$> runDB allProfilesForUpdate
    filteredProfiles <- filterM isTime profiles
    mapM_ updateName filteredProfiles

updateName :: Profile -> Handler ()
updateName (Profile name _ userId) = do
    App{twitterConsumerKey, twitterConsumerSecret} <- getYesod
    muser <- runDB $ get userId
    case muser of
        Nothing -> return ()
        (Just user) -> do
            let accessKey = t2b $ userTwitterOauthToken user
            let accessSecret = t2b $ userTwitterOauthTokenSecret user
            liftIO $ updateTwitterName name twitterConsumerKey twitterConsumerSecret accessKey accessSecret

isTime :: Profile -> Handler Bool
isTime (Profile _ profileDay userId) = do
    muser <- runDB $ get userId
    case muser of
      Nothing -> return False
      (Just user) -> (profileDay ==) <$> CT.localToday user
