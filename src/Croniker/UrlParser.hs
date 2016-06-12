module Croniker.UrlParser
    ( containsUrl
    ) where

import Prelude
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Maybe (isJust)
import Network.URI.TLD (parseTLDText)

containsUrl :: T.Text -> Bool
containsUrl = any isUrl . T.words

isUrl :: T.Text -> Bool
isUrl url = "." `T.isInfixOf` url && (isJust $ parseTLDText $ withHTTP url)

withHTTP :: T.Text -> T.Text
withHTTP t
  | "http://" `T.isPrefixOf` t = t
  | "https://" `T.isPrefixOf` t = t
  | otherwise = "http://" <> t
