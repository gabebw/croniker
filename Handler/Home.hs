module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Text.Blaze (ToMarkup, toMarkup)

instance ToMarkup Day where
  toMarkup = toMarkup . show

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (Text, Day)
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (Text, Day)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq textField (withSmallInput "Name") Nothing
    <*> areq dayField (withSmallInput "Date") Nothing
