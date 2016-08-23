module Croniker.MonikerFieldChecks
    ( runAllChecks
    )
    where

import Import

import Croniker.MonikerNormalization (normalize)
import Croniker.UrlParser (containsUrl)
import Data.Char (ord)

runAllChecks :: Field Handler Text -> Field Handler Text
runAllChecks field = foldr check field allChecks

allChecks :: [Text -> Either Text Text]
allChecks = [
              containsValidUnicodeCombinations,
              doesNotContainUrl,
              doesNotContainTwitter,
              validWhitespace . normalize,
              validLength . normalize
            ]

-- Twitter's actively working on a fix for this: https://twitter.com/bhaggs/status/767936253886992384
-- After it's fixed, this check can be deleted.
containsValidUnicodeCombinations :: Text -> Either Text Text
containsValidUnicodeCombinations moniker
    | validUnicodeCombinations moniker = Right moniker
    | otherwise = Left "Twitter requires that emoji outside the Basic Multilingual Plane be accompanied by emoji in the BMP. You can fix this by adding a heart: \10084"

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

-- Twitter only allows characters outside BMP when accompanied by characters
-- _inside_ the BMP.
validUnicodeCombinations :: Text -> Bool
validUnicodeCombinations moniker = all insideBMP moniker ||
    (any outsideBMP moniker && any insideBMP moniker)

-- Is this character outside Unicode's Basic Multilingual Plane?
outsideBMP :: Char -> Bool
outsideBMP c = ord c > 0xFFFF

-- Is this character inside Unicode's Basic Multilingual Plane?
insideBMP :: Char -> Bool
insideBMP c = ord c <= 0xFFFF
