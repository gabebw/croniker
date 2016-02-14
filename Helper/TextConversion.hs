module Helper.TextConversion
    ( b2t
    , t2b)
    where

import Prelude

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

b2t :: BSC.ByteString -> T.Text
b2t = T.pack . BSC.unpack

t2b :: T.Text -> BSC.ByteString
t2b = BSC.pack . T.unpack
