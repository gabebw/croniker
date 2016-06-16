module Helper.Request
    ( fromMaybe404
    , requireOwnedProfile
    ) where

import Import

import qualified Model.Profile as P

fromMaybe404 :: Handler (Maybe a) -> Handler a
fromMaybe404 f = maybe notFound return =<< f

requireOwnedProfile :: ProfileId -> Handler ()
requireOwnedProfile profileId = do
    userId <- requireAuthId
    void $ fromMaybe404 $ runDB $ P.findProfileFor userId profileId
