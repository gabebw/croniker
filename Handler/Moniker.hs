{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Moniker where

import Import

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Text.Blaze (ToMarkup, toMarkup)
import qualified Model.Moniker as M

instance ToMarkup Day where
  toMarkup = toMarkup . show

getMonikerR :: Handler Html
getMonikerR = do
    (Entity userId user) <- requireAuth
    today <- liftIO M.today
    tomorrow <- liftIO M.tomorrow
    (formWidget, formEnctype) <- generateFormPost (bootstrapMonikerForm tomorrow userId)
    allMonikers <- runDB $ M.allMonikers
    todaysMonikers <- runDB $ M.monikersFromToday
    defaultLayout $(widgetFile "monikers")

postMonikerR :: Handler Html
postMonikerR = do
    (Entity userId user) <- requireAuth
    tomorrow <- liftIO M.tomorrow
    ((result, formWidget), formEnctype) <- runFormPost (bootstrapMonikerForm tomorrow userId)
    case result of
        FormSuccess moniker -> do
            void $ runDB $ insert moniker
            setMessage "Moniker created"
            redirect MonikerR
        _ -> do
            setMessage "Oops, something went wrong"
            today <- liftIO M.today
            allMonikers <- runDB $ M.allMonikers
            todaysMonikers <- runDB $ M.monikersFromToday
            defaultLayout $(widgetFile "monikers")

bootstrapMonikerForm :: Day -> UserId -> Form Moniker
bootstrapMonikerForm tomorrow userId = renderBootstrap3 BootstrapBasicForm (monikerForm tomorrow userId)

monikerForm :: Day -> UserId -> AForm Handler Moniker
monikerForm tomorrow userId = Moniker
       <$> areq textField (withSmallInput "Name") Nothing
       <*> areq dateField (withSmallInput "Date") (Just tomorrow)
       <*> pure userId
    where
        dateField = check futureDate dayField
        futureDate date
            | date < tomorrow = Left ("You must select a future date" :: Text)
            | otherwise = Right date

showMonikerEntity :: Entity Moniker -> Widget
showMonikerEntity (Entity _monikerId (Moniker name date _)) = do
    [whamlet|
        <tr>
            <td>#{name}
            <td>#{date}
    |]

monikersTable :: [Entity Moniker] -> Widget
monikersTable monikers = [whamlet|
    <table .table .table-striped .table-bordered>
        <tr>
            <td>Name
            <td>Date
        $forall moniker <- monikers
            ^{showMonikerEntity moniker}
    |]
