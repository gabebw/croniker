module Croniker.MonikerFieldChecks
    ( runAllChecks
    )
    where

import Import

import Croniker.MonikerNormalization (normalize)
import Croniker.UrlParser (containsUrl)

runAllChecks :: Field Handler Text -> Field Handler Text
runAllChecks field = foldr check field allChecks

allChecks :: [Text -> Either Text Text]
allChecks = [
              doesNotContainUrl,
              doesNotContainTwitter,
              validWhitespace . normalize,
              validLength . normalize
            ]

doesNotContainTwitter :: Text -> Either Text Text
doesNotContainTwitter moniker
    | "twitter" `isInfixOf` toLower moniker = Left "Twitter doesn't allow monikers that contain \"Twitter\""
    | otherwise = Right moniker

doesNotContainUrl :: Text -> Either Text Text
doesNotContainUrl moniker
    | containsUrl moniker = Left "Twitter doesn't allow URLs in monikers"
    | otherwise = Right moniker

validLength :: Text -> Either Text Text
validLength moniker
    | length moniker == 0 = Left "Monikers cannot be blank"
    | length moniker > 20 = Left "Twitter doesn't allow monikers longer than 20 characters"
    | otherwise = Right moniker

validWhitespace :: Text -> Either Text Text
validWhitespace moniker
    | any (`isInfixOf` moniker) ["\n", "\t"] = Left "Moniker cannot contain special whitespace characters"
    | otherwise = Right moniker
