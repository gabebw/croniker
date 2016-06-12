module Handler.Settings
    ( getSettingsR
    , postSettingsR
    ) where

import Import

data FormSettings = FormSettings
    { publicRssFeed :: Bool }

getSettingsR :: Handler Html
getSettingsR = do
    euser <- requireAuth
    (widget, enctype) <- generateFormPost $ settingsForm euser
    defaultLayout $ do
        setTitle "Croniker - Settings"
        $(widgetFile "settings")

postSettingsR :: Handler Html
postSettingsR = do
    euser@(Entity userId _) <- requireAuth
    ((result, widget), enctype) <- runFormPost $ settingsForm euser

    case result of
        FormSuccess FormSettings{publicRssFeed} -> do
            void $ runDB $ update userId [UserPublicRssFeed =. publicRssFeed]
            setMessage "Settings updated"
            redirect SettingsR
        _ -> do
            setMessage "Oops, something went wrong"
            defaultLayout $ do
                setTitle "Croniker - Settings"
                $(widgetFile "settings")

settingsForm :: Entity User -> Html -> MForm Handler (FormResult FormSettings, Widget)
settingsForm (Entity userId User{userPublicRssFeed}) extra = do
    (result, view) <- mreq checkBoxField "" (Just userPublicRssFeed)
    let formSettings = FormSettings <$> result
    let widget = [whamlet|
        #{extra}
        <div class="settings-field">
            ^{fvInput view}
            <label for=#{fvId view}>
                Make
                <a href=@{FeedR userId}>your RSS feed
                public?
                <span>
            $if not userPublicRssFeed
                <div.block-tooltip>Only you can see it right now
    |]
    return (formSettings, widget)
