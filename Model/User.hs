{-# LANGUAGE NamedFieldPuns #-}

module Model.User
    ( authenticateUser
    , nextFreeDay
    , takenDays
    ) where

import Import.NoFoundation

import Data.Time.Zones.All (TZLabel(Etc__UTC))
import Database.Persist.Sql (rawSql, Single(..))

authenticateUser :: AuthId m ~ UserId => Creds m -> DB (AuthenticationResult m)
authenticateUser Creds{credsExtra} = do
    let mTwitterUserId = lookup "user_id" credsExtra
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

nextFreeDay :: UserId -> DB (Maybe (Single Day))
nextFreeDay userId = listToMaybe <$> rawSql s [toPersistValue userId]
    where
        s = "WITH next_year AS ( \
            \   SELECT current_date + s.a AS date \
            \   FROM generate_series(1,365) AS s(a) \
            \ ), joined AS ( \
            \ SELECT profile.date AS profile_date, next_year.date AS generated_date \
            \ FROM next_year LEFT OUTER JOIN profile \
            \     ON next_year.date = profile.date \
            \     AND profile.date >= current_date \
            \     AND profile.user_id = ? \
            \ ) \
            \ SELECT generated_date AS next_available_date \
            \ FROM joined \
            \ WHERE profile_date IS NULL \
            \ ORDER BY generated_date ASC \
            \ LIMIT 1;"

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
