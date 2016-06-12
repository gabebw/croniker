module Handler.Feed
    ( getFeedR
    ) where

import Import

import Helper.Request (fromMaybe404)
import Model.Profile (feedProfilesFor)
import Text.Blaze.Html5 ((!), textValue, img, li, p, ul)
import Text.Blaze.Html5.Attributes (src)
import Yesod.RssFeed

getFeedR :: UserId -> Handler RepRss
getFeedR userId = do
    user@User{userPublicRssFeed} <- fromMaybe404 $ runDB $ get userId
    if userPublicRssFeed
       then renderProfiles userId user
       else signedOut userId user

signedOut :: UserId -> User -> Handler RepRss
signedOut feedUserId feedUser = do
    (Entity loggedInUserId _) <- requireAuth
    if loggedInUserId == feedUserId
       then renderProfiles feedUserId feedUser
       else notFound

renderProfiles :: UserId -> User -> Handler RepRss
renderProfiles userId User{userTwitterUsername} = do
    profiles <- runDB $ feedProfilesFor userId
    feedFromProfiles userId userTwitterUsername $ map entityVal profiles

feedFromProfiles :: UserId -> Text -> [Profile] -> Handler RepRss
feedFromProfiles userId username profiles = do
    now <- liftIO getCurrentTime
    let firstProfile = listToMaybe profiles
    let title = "Past Twitter names for " <> username
    let updated = maybe now (\profile -> UTCTime (profileDate profile) 0) firstProfile
    entries <- mapM profileToRssEntry profiles

    rssFeed Feed
        { feedAuthor      = username
        , feedTitle       = title
        , feedDescription = toHtml title
        , feedLanguage    = "en-US"
        , feedLinkSelf    = FeedR userId
        , feedLinkHome    = RootR
        , feedUpdated     = updated
        , feedEntries     = entries
        , feedLogo        = Nothing
        }

profileToRssEntry :: Profile -> Handler (FeedEntry (Route App))
profileToRssEntry profile@Profile{profileDate} = do
    offset <- utctDayTime <$> liftIO getCurrentTime
    return FeedEntry
        { feedEntryLink    = RootR
        , feedEntryUpdated = UTCTime profileDate offset
        , feedEntryTitle   = changeSummary profile
        , feedEntryContent = changeDescription profile
        , feedEntryEnclosure = Nothing
        }

changeSummary :: Profile -> Text
changeSummary Profile{profileMoniker, profileDescription, profilePicture} = "Changed " <> toSentence changes
    where
        changes = catMaybes [ const "moniker" <$> profileMoniker
                            , const "bio" <$> profileDescription
                            , const "picture" <$> profilePicture ]

changeDescription :: Profile -> Html
changeDescription Profile{profileMoniker, profileDescription, profilePicture} =
    ul $ mapM_ li changes
    where
        changes = catMaybes [ toHtml . mappend "Changed moniker to " <$> profileMoniker
                            , toHtml . mappend "Changed bio to " <$> profileDescription
                            , pictureHtml <$> profilePicture ]

pictureHtml :: Text -> Html
pictureHtml t = do
    p "Changed picture:"
    img ! src (textValue $ "data:;base64," <> t)

toSentence :: [Text] -> Text
toSentence [] = ""
toSentence [x] = x
toSentence [x, y] = x <> " and " <> y
toSentence (x:xs) = x <> ", " <> toSentence xs
