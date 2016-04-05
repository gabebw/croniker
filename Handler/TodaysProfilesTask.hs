{-# LANGUAGE NamedFieldPuns #-}

module Handler.TodaysProfilesTask
    ( updateTodaysProfiles
    ) where

import Import

import Data.Maybe (fromJust)
import Model.Profile (allProfilesForUpdate)
import qualified Croniker.Time as CT
import TwitterClient (updateTwitterName, updateTwitterPicture)
import Helper.TextConversion

updateTodaysProfiles :: Handler ()
updateTodaysProfiles = do
    profiles <- runDB allProfilesForUpdate
    filteredProfiles <- filterM isTime profiles
    mapM_ updateProfile filteredProfiles

updateProfile :: Entity Profile -> Handler ()
updateProfile (Entity profileId (Profile name _ userId picture _)) = do
    App{twitterConsumerKey, twitterConsumerSecret} <- getYesod
    muser <- runDB $ get userId
    case muser of
        Nothing -> return ()
        (Just user) -> do
            let username = userTwitterUsername user
            let accessKey = t2b $ userTwitterOauthToken user
            let accessSecret = t2b $ userTwitterOauthTokenSecret user
            liftIO $ do
                putStrLn $ "[" ++ username ++ "] Updating name to " ++ name
                updateTwitterName name twitterConsumerKey twitterConsumerSecret accessKey accessSecret
                when (isJust picture) $ do
                    putStrLn $ "[" ++ username ++ "] Updating picture"
                    updateTwitterPicture (fromJust picture) twitterConsumerKey twitterConsumerSecret accessKey accessSecret
            runDB $ update profileId [ProfileSent =. True]

isTime :: Entity Profile -> Handler Bool
isTime (Entity _ (Profile _ profileDay userId _ _)) = do
    muser <- runDB $ get userId
    case muser of
      Nothing -> return False
      (Just user) -> (profileDay ==) <$> CT.localToday user
