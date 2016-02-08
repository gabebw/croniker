module Helper.Request
    ( fromMaybe404
    ) where

import Import

fromMaybe404 :: Handler (Maybe a) -> Handler a
fromMaybe404 f = maybe notFound return =<< f
