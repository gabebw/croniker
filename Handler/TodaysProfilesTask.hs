{-# LANGUAGE NamedFieldPuns #-}

module Handler.TodaysProfilesTask
    ( updateTodaysProfiles
    ) where

import Import

import qualified Control.Exception.Lifted as EL
import Data.Maybe (fromJust)

import Model.Profile (allProfilesForUpdate)
import qualified Croniker.Time as CT
import TwitterClient (updateTwitterName, updateTwitterPicture)
import Helper.HttpExceptionHandler (handleHttpException)
import Helper.TextConversion

updateTodaysProfiles :: Handler ()
updateTodaysProfiles = do
    profiles <- runDB allProfilesForUpdate
    filteredProfiles <- filterM isTime profiles
    mapM_ updateWithErrorHandling filteredProfiles

updateWithErrorHandling :: Entity Profile -> Handler ()
updateWithErrorHandling profile = EL.try (updateProfile profile)
    >>= handleHttpException

updateProfile :: Entity Profile -> Handler ()
updateProfile (Entity profileId (Profile{profileName, profileUserId, profilePicture})) = do
    muser <- runDB $ get profileUserId
    case muser of
        Nothing -> return ()
        (Just user) -> do
            App{twitterConsumerKey, twitterConsumerSecret} <- getYesod
            let username = userTwitterUsername user
            let accessKey = t2b $ userTwitterOauthToken user
            let accessSecret = t2b $ userTwitterOauthTokenSecret user
            liftIO $ do
                logger username $ "Updating name to " <> profileName
                updateTwitterName profileName twitterConsumerKey twitterConsumerSecret accessKey accessSecret
                when (isJust profilePicture) $ do
                    logger username "Updating picture"
                    updateTwitterPicture (fromJust profilePicture) twitterConsumerKey twitterConsumerSecret accessKey accessSecret
            runDB $ update profileId [ProfileSent =. True]

logger :: Text -> Text -> IO ()
logger username t = putStrLn $ "[" <> username <> "] " <> t

isTime :: Entity Profile -> Handler Bool
isTime (Entity _ (Profile{profileDate, profileUserId})) = do
    muser <- runDB $ get profileUserId
    case muser of
      Nothing -> return False
      (Just user) -> (profileDate ==) <$> CT.localToday user
