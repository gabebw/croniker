{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Moniker where

import Import

import Helper.Request (fromMaybe404)
import Text.Blaze (ToMarkup, toMarkup)
import qualified Model.Moniker as M
import Data.Maybe (fromJust)

instance ToMarkup Day where
  toMarkup = toMarkup . show

getMonikerR :: Handler Html
getMonikerR = do
    (Entity userId _) <- requireAuth
    tomorrow <- liftIO M.tomorrow
    (formWidget, formEnctype) <- generateFormPost (monikerForm tomorrow userId)
    monikersTemplate userId formWidget formEnctype

postMonikerR :: Handler Html
postMonikerR = do
    (Entity userId _) <- requireAuth
    tomorrow <- liftIO M.tomorrow
    ((result, formWidget), formEnctype) <- runFormPost (monikerForm tomorrow userId)
    case result of
        FormSuccess moniker -> do
            void $ runDB $ insert moniker
            setMessage "Moniker created"
            redirect MonikerR
        _ -> do
            setMessage "Oops, something went wrong"
            monikersTemplate userId formWidget formEnctype

monikersTemplate :: (ToWidget App fw) => UserId -> fw -> Enctype -> Handler Html
monikersTemplate userId formWidget formEnctype = do
    csrfToken <- fromJust . reqToken <$> getRequest
    today <- liftIO M.today
    allMonikers <- runDB $ M.futureMonikersFor userId
    defaultLayout $ do
        setTitle "Croniker"
        $(widgetFile "monikers")

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

monikerForm :: Day -> UserId -> Form Moniker
monikerForm tomorrow userId = renderDivs $ Moniker
       <$> areq nameField (fs "New Name" [("maxlength", "20")]) Nothing
       <*> areq dateField (fs "Date" []) (Just tomorrow)
       <*> pure userId
    where
        dateField = check futureDate dayField
        nameField = check maxLength textField
        futureDate :: Day -> Either Text Day
        futureDate date
            | date < tomorrow = Left "You must select a future date"
            | otherwise = Right date
        maxLength :: Text -> Either Text Text
        maxLength name
            | length name > 20 = Left "Twitter doesn't allow monikers longer than 20 characters"
            | otherwise = Right name
        fs :: Text -> [(Text, Text)] -> FieldSettings site
        fs label attrs = FieldSettings
            { fsLabel = SomeMessage label
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = attrs
            }
