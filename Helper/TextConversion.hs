module Helper.TextConversion
    ( b2t
    , t2b)
    where

import Prelude

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

b2t :: BSC.ByteString -> T.Text
b2t = T.decodeLatin1

t2b :: T.Text -> BSC.ByteString
t2b = T.encodeUtf8
