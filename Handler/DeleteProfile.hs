{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.DeleteProfile
    ( postDeleteProfileR
    ) where

import Import

import Helper.Request (requireOwnedProfile)

postDeleteProfileR :: ProfileId -> Handler ()
postDeleteProfileR profileId = do
    requireOwnedProfile profileId
    runDB $ delete profileId
    setMessage "Profile deleted!"
    redirect ProfileR
