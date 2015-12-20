module Handler.Moniker where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Text.Blaze (ToMarkup, toMarkup)

instance ToMarkup Day where
  toMarkup = toMarkup . show

getMonikerR :: Handler Html
getMonikerR = do
    (formWidget, formEnctype) <- generateFormPost monikerForm
    let submission = Nothing :: Maybe (Text, Day)
    defaultLayout $ do
        setTitle "Croniker"
        $(widgetFile "monikers")

postMonikerR :: Handler Html
postMonikerR = do
    ((result, formWidget), formEnctype) <- runFormPost monikerForm
    let submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        setTitle "Croniker"
        $(widgetFile "monikers")

monikerForm :: Form (Text, Day)
monikerForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq textField (withSmallInput "Name") Nothing
    <*> areq dayField (withSmallInput "Date") Nothing
