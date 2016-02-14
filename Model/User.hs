module Model.User
    ( authenticateUser
    ) where

import Import.NoFoundation

import Data.Time.Zones.All (TZLabel(Etc__UTC))

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

defaultTZLabel :: TZLabel
defaultTZLabel = Etc__UTC
