module Croniker.Types
    ( OauthCredentials(OauthCredentials)
    , OauthReader
    , runOauthReader
    ) where

import Import.NoFoundation

import qualified Network.Wreq as W

data OauthCredentials = OauthCredentials
    { oauthKey :: ByteString
    , oauthSecret :: ByteString
    , oauthUserKey :: ByteString
    , oauthUserSecret :: ByteString
    }

type OauthReader = ReaderT W.Auth IO

runOauthReader :: MonadIO m => OauthReader a -> OauthCredentials -> m a
runOauthReader reader creds = liftIO $ runReaderT reader (toOauth1Auth creds)

toOauth1Auth :: OauthCredentials -> W.Auth
toOauth1Auth OauthCredentials{oauthKey, oauthSecret, oauthUserKey, oauthUserSecret} = W.oauth1Auth
    oauthKey
    oauthSecret
    oauthUserKey
    oauthUserSecret
