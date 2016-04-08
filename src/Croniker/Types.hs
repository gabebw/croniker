{-# LANGUAGE NamedFieldPuns #-}

module Croniker.Types
    ( OauthCredentials(OauthCredentials)
    , toOauth1Auth
    ) where

import Import.NoFoundation

import qualified Network.Wreq as W

data OauthCredentials = OauthCredentials
    { oauthKey :: ByteString
    , oauthSecret :: ByteString
    , oauthUserKey :: ByteString
    , oauthUserSecret :: ByteString
    }

toOauth1Auth :: OauthCredentials -> W.Auth
toOauth1Auth (OauthCredentials{oauthKey, oauthSecret, oauthUserKey, oauthUserSecret}) = W.oauth1Auth
    oauthKey
    oauthSecret
    oauthUserKey
    oauthUserSecret
