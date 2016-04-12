{-# LANGUAGE NamedFieldPuns #-}

module Handler.UpdateProfilesTask
    ( updateTodaysProfiles
    , updateAllProfiles
    ) where

import Import

import qualified Control.Exception.Lifted as EL
import Croniker.Types (OauthCredentials(OauthCredentials), OauthReader, runOauthReader)

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

updateAllProfiles :: Handler ()
updateAllProfiles = do
    profiles <- runDB $ selectList [] []
    mapM_ updateWithErrorHandling profiles

updateWithErrorHandling :: Entity Profile -> Handler ()
updateWithErrorHandling profile = EL.try (updateProfile profile)
    >>= handleHttpException

updateProfile :: Entity Profile -> Handler ()
updateProfile (Entity profileId (Profile{profileName, profileUserId, profilePicture})) = do
    muser <- runDB $ get profileUserId
    case muser of
        Nothing -> return ()
        (Just user) -> do
            let username = userTwitterUsername user
            let computation = do
                updateName profileName >>= logger username
                forM_ profilePicture $ \picture ->
                    logger username =<< updatePicture picture
            runOauthReader computation =<< oauthCredentials user
            runDB $ update profileId [ProfileSent =. True]

oauthCredentials :: User -> Handler OauthCredentials
oauthCredentials user = do
    App{twitterConsumerKey, twitterConsumerSecret} <- getYesod
    let accessKey = t2b $ userTwitterOauthToken user
    let accessSecret = t2b $ userTwitterOauthTokenSecret user
    return $ OauthCredentials
        twitterConsumerKey
        twitterConsumerSecret
        accessKey
        accessSecret

logger :: MonadIO m => Text -> Text -> m ()
logger username t = putStrLn $ "[" <> username <> "] " <> t

updateName :: Text -> OauthReader Text
updateName profileName = do
    updateTwitterName profileName
    return $ "Updating name to " <> profileName

updatePicture :: Text -> OauthReader Text
updatePicture profilePicture = do
    updateTwitterPicture profilePicture
    return "Updating picture"

isTime :: Entity Profile -> Handler Bool
isTime (Entity _ (Profile{profileDate, profileUserId})) = do
    muser <- runDB $ get profileUserId
    case muser of
      Nothing -> return False
      (Just user) -> (profileDate ==) <$> CT.localToday user
