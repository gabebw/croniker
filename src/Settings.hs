{-# Language CPP #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod hiding  (throw)
import Control.Exception           (throw)
import Data.Aeson                  (Result (..), fromJSON, withObject, (.!=),
                                    (.:?))
import Data.FileEmbed              (embedFile)
import Data.Yaml                   (decodeEither')
import Database.Persist.Postgresql (PostgresConf)
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
import Network.Wai.Handler.Warp    (HostPreference)
import Yesod.Default.Config2       (applyEnvValue, configSettingsYml)
#if DEVELOPMENT
import Yesod.Default.Util (WidgetFileSettings, widgetFileReload)
#else
import Yesod.Default.Util (WidgetFileSettings, widgetFileNoReload)
#endif
import Web.Heroku.Persist.Postgresql (fromDatabaseUrl)

-- | Actions which only require access to the database connection can be given
--   type @DB a@ (as opposed to @YesodDB App a@). This allows them to also be
--   called in tests.
--   Stolen from https://github.com/thoughtbot/carnival
type DB a = forall (m :: * -> *).
    (MonadIO m, Functor m) => ReaderT SqlBackend m a

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    , appDatabaseConf           :: PostgresConf
    -- ^ Configuration settings for accessing the database.
    , appRoot                   :: Maybe Text
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appMutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining
    , appLogLevel               :: LogLevel
    -- ^ The LogLevel to use
    , appUseDummyAuth :: Bool
    -- ^ Use dummy authentication? (Only set in tests.)
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#if DEVELOPMENT
                True
#else
                False
#endif

        -- This value is needed in a pure context, and so can't read from ENV.
        -- It also doesn't differ between environments, so we might as well
        -- harcode it.
        let appStaticDir = "static"

        appDatabaseConf           <- fromDatabaseUrl
            <$> o .: "database-pool-size"
            <*> o .: "database-url"
        appRoot                   <- o .:? "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"
        appMutableStatic          <- o .:? "mutable-static"   .!= defaultDev
        appSkipCombining          <- o .: "skip-combining"
        appLogLevel               <- parseLogLevel <$> o .: "log-level"
        appUseDummyAuth           <- o .: "use-dummy-auth"

        return AppSettings {..}
        where
            parseLogLevel :: Text -> LogLevel
            parseLogLevel t = case toLower t of
                "debug" -> LevelDebug
                "info" -> LevelInfo
                "warn" -> LevelWarn
                "error" -> LevelError
                _ -> LevelOther t

allowsLevel :: AppSettings -> LogLevel -> Bool
AppSettings{appLogLevel} `allowsLevel` level = level >= appLogLevel

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile =
#if DEVELOPMENT
    widgetFileReload
#else
    widgetFileNoReload
#endif
    def

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either throw id $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings
