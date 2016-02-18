{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.DeleteMoniker
    ( postDeleteMonikerR
    ) where

import Import

import Helper.Request (fromMaybe404)
import qualified Model.Moniker as M

postDeleteMonikerR :: MonikerId -> Handler ()
postDeleteMonikerR monikerId = do
    requireOwnedMoniker monikerId
    runDB $ delete monikerId
    setMessage "Moniker deleted!"
    redirect MonikerR

requireOwnedMoniker :: MonikerId -> Handler ()
requireOwnedMoniker monikerId = do
    userId <- requireAuthId
    void $ fromMaybe404 $ runDB $ M.findMonikerFor userId monikerId
