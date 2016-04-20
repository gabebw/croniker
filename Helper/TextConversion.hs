module Helper.TextConversion
    ( b2t
    , base64Encode
    , t2b)
    where

import Prelude

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

b2t :: BSC.ByteString -> T.Text
b2t = T.decodeLatin1

t2b :: T.Text -> BSC.ByteString
t2b = T.encodeUtf8

base64Encode :: BSL.ByteString -> T.Text
base64Encode = b2t . B64.encode . BSL.toStrict
