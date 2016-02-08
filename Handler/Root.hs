module Handler.Root where

import Import
import Yesod.Auth.OAuth (twitterUrl)

getRootR :: Handler Html
getRootR = do
    maid <- maybeAuthId

    case maid of
        Nothing -> defaultLayout $ do
            setTitle "Carnival"
            $(widgetFile "root")
        (Just  _) -> redirect MonikerR
