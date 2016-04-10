module Croniker.UrlParser
    ( containsUrl
    ) where

import Prelude
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Either (isRight)
import Text.ParserCombinators.Parsec

type Url = String

containsUrl :: T.Text -> Bool
containsUrl t = or $ map isUrl (words $ T.unpack $ T.toLower t)

isUrl :: String -> Bool
isUrl s = isRight $ parse url "" s

url :: Parser Url
url = do
    a <- many1 alphaNum
    b <- string "."
    c <- tld
    return $ a <> b <> c

-- Twitter allows most 2-letter TLDs, but not `.co`
tld :: Parser String
tld = try (count 3 alphaNum)
    <|> string "co"
