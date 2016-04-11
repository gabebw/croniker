module Croniker.UrlParser
    ( containsUrl
    ) where

import Prelude
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Either (isRight)
import Text.Parsec
import Text.Parsec.Text (Parser)

type Url = String

containsUrl :: T.Text -> Bool
containsUrl t = or $ map isUrl (T.words $ T.toLower t)

isUrl :: T.Text -> Bool
isUrl = isRight . parse url ""

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
