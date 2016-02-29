{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Moniker
    ( getMonikerR
    , postDeleteMonikerR
    , postMonikerR
    ) where

import Import

import Data.Maybe (fromJust)
import Data.Time.Format (FormatTime)
import Text.Blaze (ToMarkup, toMarkup)
import Helper.Request (fromMaybe404)
import qualified Model.Moniker as M
import qualified Croniker.Time as CT

instance ToMarkup Day where
  toMarkup = toMarkup . show

prerequisites :: Handler (Entity User)
prerequisites = do
    euser@(Entity _ user) <- requireAuth
    requireSetTimezone user
    return euser

getMonikerR :: Handler Html
getMonikerR = do
    euser@(Entity userId user) <- prerequisites
    tomorrow <- CT.localTomorrow user
    monikerFormPost <- generateFormPost $ monikerForm tomorrow userId
    monikersTemplate euser monikerFormPost

requireSetTimezone :: User -> Handler ()
requireSetTimezone user = when (not $ userChoseTimezone user) (redirect ChooseTimezoneR)

postMonikerR :: Handler Html
postMonikerR = do
    euser@(Entity userId user) <- prerequisites
    tomorrow <- CT.localTomorrow user
    ((result, formWidget), formEnctype) <- runFormPost $ monikerForm tomorrow userId
    case result of
        FormSuccess moniker -> do
            void $ runDB $ insert moniker
            setMessage "Moniker created"
            redirect MonikerR
        _ -> do
            setMessage "Oops, something went wrong"
            monikersTemplate euser (formWidget, formEnctype)

monikersTemplate :: (ToWidget App w) => Entity User -> (w, Enctype) -> Handler Html
monikersTemplate (Entity userId _) (monikerWidget, monikerEnc) = do
    csrfToken <- fromJust . reqToken <$> getRequest
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
       <$> areq nameField (fs "New moniker" [("maxlength", "20")]) Nothing
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

prettyTime :: (FormatTime t) => t -> String
prettyTime = formatTime defaultTimeLocale "%B %d, %Y"
