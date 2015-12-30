module Handler.Moniker where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Text.Blaze (ToMarkup, toMarkup)

instance ToMarkup Day where
  toMarkup = toMarkup . show

getMonikerR :: Handler Html
getMonikerR = do
    (formWidget, formEnctype) <- generateFormPost bootstrapMonikerForm
    day <- today
    allMonikers <- runDB $ selectList [] [Asc MonikerDate]
    todaysMonikers <- runDB $ selectList [MonikerDate ==. day] [Asc MonikerDate]
    defaultLayout $(widgetFile "monikers")

postMonikerR :: Handler Html
postMonikerR = do
    ((result, formWidget), formEnctype) <- runFormPost bootstrapMonikerForm
    case result of
        FormSuccess moniker -> do
            void $ runDB $ insert moniker
            setMessage "Moniker created"
            redirect MonikerR
        _ -> do
            setMessage "Oops, something went wrong"
            day <- today
            allMonikers <- runDB $ selectList [] [Asc MonikerDate]
            todaysMonikers <- runDB $ selectList [MonikerDate ==. day] [Asc MonikerDate]
            defaultLayout $(widgetFile "monikers")


monikerForm :: AForm Handler Moniker
monikerForm = Moniker
       <$> areq textField (withSmallInput "Name") Nothing
       <*> areq dayField (withSmallInput "Date") Nothing

bootstrapMonikerForm :: Form Moniker
bootstrapMonikerForm = renderBootstrap3 BootstrapBasicForm monikerForm

showMonikerEntity :: Entity Moniker -> Widget
showMonikerEntity (Entity _monikerId (Moniker name date)) = do
    [whamlet|
        <tr>
            <td>#{name}
            <td>#{date}
    |]

today :: HandlerT App IO Day
today = liftIO $ getCurrentTime >>= return . utctDay
