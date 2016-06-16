module Handler.UpdateProfilesTask
    ( updateTodaysProfiles
    , updateAllProfiles
    ) where

import Import

import qualified Control.Exception.Lifted as EL
import Croniker.Types (OauthCredentials(OauthCredentials), OauthReader, runOauthReader)

import Model.Profile (allProfilesForUpdate)
import qualified Croniker.Time as CT
import TwitterClient (updateTwitterDescription, updateTwitterMoniker, updateTwitterPicture)
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
updateProfile (Entity profileId (Profile{profileDescription, profileMoniker, profileUserId, profilePicture})) = do
    muser <- runDB $ get profileUserId
    case muser of
        Nothing -> return ()
        (Just user) -> do
            let username = userTwitterUsername user
            let computation = do
                forM_ profileMoniker $ \moniker ->
                    updateMoniker moniker >>= logger username
                forM_ profileDescription $ \description ->
                    updateDescription description >>= logger username
                forM_ profilePicture $ \picture ->
                    updatePicture picture >>= logger username
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

updateMoniker :: Text -> OauthReader Text
updateMoniker profileMoniker = do
    updateTwitterMoniker profileMoniker
    return $ "Updating moniker to " <> profileMoniker

updateDescription :: Text -> OauthReader Text
updateDescription profileDescription = do
    updateTwitterDescription profileDescription
    return $ "Updating description to " <> profileDescription

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
