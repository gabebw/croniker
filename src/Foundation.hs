{-# LANGUAGE InstanceSigs #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.OAuth     (authTwitterUsingUserId)
import Yesod.Auth.Dummy     (authDummy)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

import Model.User (authenticateUser)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings           :: AppSettings
    , appStatic             :: Static -- ^ Settings for static file serving.
    , appConnPool           :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager        :: Manager
    , appLogger             :: Logger
    , twitterConsumerKey    :: ByteString
    , twitterConsumerSecret :: ByteString
    , googleApiKey          :: String
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req -> getApprootText guessApproot app req

    -- Store session data on the client in encrypted cookies.
    -- `envClientSessionBackend` will use the `$SESSION_KEY` environment
    -- variable, or if that's not set (e.g. in development), it will generate
    -- one and print it to STDOUT.
    makeSessionBackend _ = Just <$> envClientSessionBackend oneWeek "SESSION_KEY"
        where
            oneWeek = 60 * 24 * 7

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- The defaultCsrfMiddleware:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware

    defaultLayout widget = do
        master <- getYesod
        mmessage <- getMessage
        muser <- maybeAuth

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_application_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized RootR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized (FeedR _) _ = return Authorized
    isAuthorized _ _ = isSignedIn

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- Log messages only if app settings allow the message's log level.
    shouldLogIO App{appSettings} _source level = return $ appSettings `allowsLevel` level

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = ProfileR
    -- Where to send a user after logout
    logoutDest _ = ProfileR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate Creds{credsIdent, credsExtra} = do
        App{appSettings} <- getYesod
        liftHandler $ case appUseDummyAuth appSettings of
            True -> runDB $ authenticateUser (Just credsIdent) []
            False -> runDB $ authenticateUser (lookup "user_id" credsExtra) credsExtra

    authPlugins app =
        case appUseDummyAuth $ appSettings app of
            True -> [authDummy]
            False -> [authTwitterUsingUserId (twitterConsumerKey app) (twitterConsumerSecret app)]

    loginHandler = liftHandler $ redirect RootR

isSignedIn :: Handler AuthResult
isSignedIn = do
    maid <- maybeAuthId
    return $ case maid of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
