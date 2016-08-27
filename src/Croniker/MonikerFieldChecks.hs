module Croniker.MonikerFieldChecks
    ( runAllChecks
    , allChecks
    )
    where

import Import

import Croniker.UrlParser (containsUrl)
import Data.Char (chr, ord)
import Data.Text (dropAround)

runAllChecks :: Field Handler Text -> Field Handler Text
runAllChecks field = foldr check field allChecks

allChecks :: [Text -> Either Text Text]
allChecks = [
              doesNotContainUrl,
              doesNotContainTwitter,
              validWhitespace,
              validCharacters,
              validLength
            ]

doesNotContainTwitter :: Text -> Either Text Text
doesNotContainTwitter moniker
    | "twitter" `isInfixOf` toLower moniker = Left "Monikers can't contain \"Twitter\""
    | otherwise = Right moniker

doesNotContainUrl :: Text -> Either Text Text
doesNotContainUrl moniker
    | containsUrl moniker = Left "Monikers can't contain URLs"
    | otherwise = Right moniker

validLength :: Text -> Either Text Text
validLength moniker
    | length moniker == 0 = Left "Monikers can't be blank"
    | length moniker > 20 = Left "Monikers can't be longer than 20 characters"
    | otherwise = Right moniker

validWhitespace :: Text -> Either Text Text
validWhitespace moniker
    | moniker `hasAnyChars` whitespace = Left "Moniker can't contain special whitespace characters"
    | otherwise = Right moniker
    where
        whitespace = map chr [0x202A..0x202F] ++ ['\n', '\t']

validCharacters :: Text -> Either Text Text
validCharacters moniker
    -- Characters that aren't allowed anywhere in monikers.
    | moniker `hasAnyChars` ['<', '>'] = Left "Moniker cannot contain \"<\" or \">\""
    | dropAround charactersNotAllowedAtBeginningOrEnd moniker /= moniker = Left "Moniker can't start or end with U+2028 or U+2029"
    | otherwise = Right moniker

hasAnyChars :: Text -> [Char] -> Bool
hasAnyChars t cs = any (`elem` cs) t

-- Characters that are not allowed at the beginning/end of monikers (but are
-- allowed when surrounded by other characters).
charactersNotAllowedAtBeginningOrEnd :: Char -> Bool
charactersNotAllowedAtBeginningOrEnd char = ord char `elem` [0x2028, 0x2029]
