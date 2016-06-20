module Model.User
    ( authenticateUser
    , nextFreeDay
    , takenDays
    ) where

import Import.NoFoundation

import Data.Time.Calendar (addDays)
import Data.Time.Zones.All (TZLabel(Etc__UTC))

authenticateUser :: AuthId m ~ UserId => Maybe Text -> [(Text, Text)] -> DB (AuthenticationResult m)
authenticateUser mTwitterUserId credsExtra = do
    muser <- maybe (return Nothing) (getBy . UniqueUser) mTwitterUserId
    case muser of
        Nothing -> createUser (credsToUser credsExtra)
        Just (Entity userId _) -> updateUser userId
    where
        createUser :: AuthId m ~ UserId => Maybe User -> DB (AuthenticationResult m)
        createUser (Just user) = Authenticated <$> insert user
        createUser Nothing = return $ ServerError "Something went wrong"

        updateUser :: AuthId m ~ UserId => UserId -> DB (AuthenticationResult m)
        updateUser userId = do
            update userId (updateStatements credsExtra)
            return (Authenticated userId)

takenDays :: Day -> UserId -> DB [Day]
takenDays tomorrow userId = map (profileDate . entityVal) <$> selectList [ProfileUserId ==. userId, ProfileDate >=. tomorrow] []

nextFreeDay :: Day -> [Day] -> DB Day
nextFreeDay tomorrow takenDates = do
    return $ fromMaybe tomorrow $ find (`onotElem` takenDates) nextYear
    where
        nextYear = map (`addDays` tomorrow) [0..365]

credsToUser :: [(Text, Text)] -> Maybe User
credsToUser credsExtra = User
    <$> lookup "user_id" credsExtra
    <*> lookup "screen_name" credsExtra
    <*> lookup "oauth_token" credsExtra
    <*> lookup "oauth_token_secret" credsExtra
    <*> pure defaultTZLabel
    <*> pure False

updateStatements :: [(Text, Text)] -> [Update User]
updateStatements credsExtra = catMaybes
    [ (UserTwitterOauthToken =.) <$> lookup "oauth_token" credsExtra
    , (UserTwitterOauthTokenSecret =.) <$> lookup "oauth_token_secret" credsExtra
    ]

defaultTZLabel :: TZLabel
defaultTZLabel = Etc__UTC
