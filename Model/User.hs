module Model.User
    ( authenticateUser
    , localTime
    ) where

import Import.NoFoundation

import Data.Time.LocalTime
import Data.Time.Zones
import Data.Time.Zones.All(TZLabel(Etc__UTC), tzByLabel)

authenticateUser :: AuthId m ~ UserId => Creds m -> DB (AuthenticationResult m)
authenticateUser Creds{..} = do
    let mTwitterUserId = (lookup "user_id" credsExtra)
    muser <- maybe (return Nothing) (getBy . UniqueUser) mTwitterUserId
    case muser of
        Nothing -> createUser (credsToUser credsExtra)
        (Just user) -> return $ Authenticated $ entityKey user
    where
        createUser :: AuthId m ~ UserId => Maybe User -> DB (AuthenticationResult m)
        createUser (Just user) = Authenticated <$> insert user
        createUser Nothing = return $ ServerError "Something went wrong"

credsToUser :: [(Text, Text)] -> Maybe User
credsToUser credsExtra = User
    <$> (lookup "user_id" credsExtra)
    <*> (lookup "screen_name" credsExtra)
    <*> (lookup "oauth_token" credsExtra)
    <*> (lookup "oauth_token_secret" credsExtra)
    <*> pure defaultTZLabel
    <*> pure False

defaultTZLabel :: TZLabel
defaultTZLabel = Etc__UTC

localTime :: UTCTime -> User -> LocalTime
localTime utcNow user = utcToLocalTime timezone utcNow
    where
        timezone = timeZoneForUTCTime tz utcNow
        tz = tzByLabel $ userTzLabel user
