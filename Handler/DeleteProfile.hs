{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.DeleteProfile
    ( postDeleteProfileR
    ) where

import Import

import Helper.Request (fromMaybe404)
import qualified Model.Profile as P

postDeleteProfileR :: ProfileId -> Handler ()
postDeleteProfileR profileId = do
    requireOwnedProfile profileId
    runDB $ delete profileId
    setMessage "Profile deleted!"
    redirect ProfileR

requireOwnedProfile :: ProfileId -> Handler ()
requireOwnedProfile profileId = do
    userId <- requireAuthId
    void $ fromMaybe404 $ runDB $ P.findProfileFor userId profileId
