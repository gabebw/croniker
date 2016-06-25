module Croniker.MonikerNormalization
    ( normalize
    )
    where

import ClassyPrelude
import Data.Char (ord)
import qualified Data.Text as T

normalize :: Text -> Text
normalize = T.strip . stripCharacters . removeDisallowedCharacters

-- Remove characters that aren't allowed anywhere in monikers.
removeDisallowedCharacters :: Text -> Text
removeDisallowedCharacters = filter (not . bad)
    where
        bad c = c `elem` ['<', '>'] || fancyUnicodeWhitespace c || outsideBMP c

-- Is this character outside Unicode's Basic Multilingual Plane (which ends at
-- U+FEFF)?
outsideBMP :: Char -> Bool
outsideBMP c = ord c >= 0xFEFF

-- Is this character fancy Unicode whitespace?
fancyUnicodeWhitespace :: Char -> Bool
fancyUnicodeWhitespace c = ord c `elem` [0x202A..0x202F]

-- Remove characters that are not allowed at the beginning/end of monikers (but
-- are allowed when surrounded by other characters).
stripCharacters :: Text -> Text
stripCharacters = T.dropAround bad
    where
        bad c = ord c `elem` [0x2028, 0x2029]
