module Model.User
    ( authenticateUser
    , findSessionUser
    ) where

import Import.NoFoundation

import Data.Maybe (fromJust)

authenticateUser :: AuthId m ~ UserId => Creds m -> DB (AuthenticationResult m)
authenticateUser Creds{..} = do
    let twitterUserId = fromJust $ lookup "user_id" credsExtra
    muser <- getBy $ UniqueUser twitterUserId
    case muser of
      Nothing -> Authenticated <$> insert (credsToUser credsExtra)
      (Just user) -> return $ Authenticated $ entityKey user

credsToUser :: [(Text, Text)] -> User
credsToUser credsExtra = fromJust $ User
    <$> (lookup "user_id" credsExtra)
    <*> (lookup "screen_name" credsExtra)
    <*> (lookup "oauth_token" credsExtra)
    <*> (lookup "oauth_token_secret" credsExtra)

findSessionUser :: Maybe Text -> DB (Maybe UserId)
findSessionUser mTwitterId = do
    muser <- maybe (return Nothing) (getBy . UniqueUser) mTwitterId
    return $ maybe Nothing (Just . entityKey) muser