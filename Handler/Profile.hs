{-# OPTIONS_GHC -fno-warn-orphans #-}

module Handler.Profile
    ( getProfileR
    , postProfileR
    , postDeleteProfileR
    ) where

import Import

import Data.Maybe (fromJust)
import Data.Time.Format (FormatTime)
import Text.Blaze (ToMarkup, toMarkup)

import Form.Profile (profileForm)
import Helper.Request (requireOwnedProfile)
import qualified Croniker.Time as CT
import qualified Model.Profile as P
import qualified Model.FormProfile as FP
import qualified Model.User as U

instance ToMarkup Day where
  toMarkup = toMarkup . show

getProfileR :: Handler Html
getProfileR = do
    euser@(Entity userId user) <- prerequisites
    tomorrow <- CT.localTomorrow user
    takenDays <- runDB $ U.takenDays tomorrow userId
    nextFreeDay <- runDB $ U.nextFreeDay tomorrow takenDays
    widget <- fst <$> (generateFormPost $ profileForm nextFreeDay takenDays tomorrow)
    profilesTemplate euser widget

postProfileR :: Handler Html
postProfileR = do
    euser@(Entity userId user) <- prerequisites
    tomorrow <- CT.localTomorrow user
    takenDays <- runDB $ U.takenDays tomorrow userId
    nextFreeDay <- runDB $ U.nextFreeDay tomorrow takenDays
    (result, formWidget) <- fst <$> (runFormPost $ profileForm nextFreeDay takenDays tomorrow)

    case result of
        FormSuccess formProfile -> do
            if FP.valuesArePresent formProfile
               then do
                   FP.addProfile userId formProfile
                   setMessage "Profile created"
                   redirect ProfileR
                else do
                   setMessage "Please set a moniker, description, or picture"
                   profilesTemplate euser formWidget
        _ -> do
            setMessage "Oops, something went wrong"
            profilesTemplate euser formWidget

postDeleteProfileR :: ProfileId -> Handler ()
postDeleteProfileR profileId = do
    requireOwnedProfile profileId
    runDB $ delete profileId
    setMessage "Profile deleted!"
    redirect ProfileR

prerequisites :: Handler (Entity User)
prerequisites = do
    euser@(Entity _ user) <- requireAuth
    requireSetTimezone user
    return euser

requireSetTimezone :: User -> Handler ()
requireSetTimezone user = unless (userChoseTimezone user) (redirect ChooseTimezoneR)

profilesTemplate :: (ToWidget App w) => Entity User -> w -> Handler Html
profilesTemplate euser profileWidget = do
    csrfToken <- fromJust . reqToken <$> getRequest
    allProfiles <- runDB $ P.todayAndFutureProfilesFor euser
    defaultLayout $ do
        setTitle "Croniker"
        $(widgetFile "profiles")

textFieldsTemplate :: Profile -> Widget
textFieldsTemplate Profile{profileMoniker, profileDescription} = [whamlet|
        $maybe name <- profileMoniker
            <p>
                <strong>#{name}
        $maybe description <- profileDescription
            <p>Bio: #{description}
    |]

prettyTime :: (FormatTime t) => t -> String
prettyTime = formatTime defaultTimeLocale "%B %d, %Y"
