module Handler.UpdateProfilesTask
    ( updateTodaysProfiles
    , updateAllProfiles
    ) where

import Import

import UnliftIO.Exception (try)
import Croniker.Types (OauthCredentials(OauthCredentials), OauthReader, runOauthReader)

import Model.Profile (allProfilesForUpdate)
import qualified Croniker.Time as CT
import TwitterClient (updateTwitterDescription, updateTwitterMoniker, updateTwitterPicture)
import Helper.HttpExceptionHandler (handleHttpException)
import Helper.TextConversion

type Logger = Text -> OauthReader ()
type Updater = Text -> OauthReader Text

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
updateWithErrorHandling profile = try (updateProfile profile)
    >>= handleHttpException

updateProfile :: Entity Profile -> Handler ()
updateProfile (Entity profileId Profile{profileDescription, profileMoniker, profileUserId, profilePicture}) = do
    muser <- runDB $ get profileUserId
    case muser of
        Nothing -> return ()
        (Just user) -> do
            let l = logger $ userTwitterUsername user
            let pairs = [ (profileMoniker, updateMoniker)
                        , (profileDescription, updateDescription)
                        , (profilePicture, updatePicture) ]
            let computation = mapM_ (updateAndLog l) pairs
            runOauthReader computation =<< oauthCredentials user
            runDB $ update profileId [ProfileSent =. True]

updateAndLog :: Logger -> (Maybe Text, Updater) -> OauthReader ()
updateAndLog l (mvalue, f) = forM_ mvalue $ \value -> f value >>= l

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

logger :: Text -> Logger
logger username t = putStrLn $ "[" <> username <> "] " <> t

updateMoniker :: Updater
updateMoniker profileMoniker = do
    updateTwitterMoniker profileMoniker
    return $ "Updating moniker to " <> profileMoniker

updateDescription :: Updater
updateDescription profileDescription = do
    updateTwitterDescription profileDescription
    return $ "Updating description to " <> profileDescription

updatePicture :: Updater
updatePicture profilePicture = do
    updateTwitterPicture profilePicture
    return "Updating picture"

isTime :: Entity Profile -> Handler Bool
isTime (Entity _ Profile{profileDate, profileUserId}) = do
    muser <- runDB $ get profileUserId
    case muser of
      Nothing -> return False
      (Just user) -> (profileDate ==) <$> CT.localToday user
