module Croniker.MonikerNormalization
    ( normalize
    )
    where

import ClassyPrelude
import Data.Char (ord)
import qualified Data.Text as T

normalize :: Text -> Text
normalize = T.strip . stripCharacters . removeInvalidUnicode

-- Twitter strips out fancy Unicode whitespace characters, and doesn't allow
-- anything outside the Basic Multilingual Plane (which ends at U+FEFF).
removeInvalidUnicode :: Text -> Text
removeInvalidUnicode = filter (not . bad)
    where
        bad c = ord c `elem` [0x202A..0x202F] || ord c >= 0xFEFF

-- Strip characters that are not allowed at the beginning/end of monikers (but
-- are allowed when surrounded by other characters).
stripCharacters :: Text -> Text
stripCharacters = T.dropWhile bad . T.dropWhileEnd bad
    where
        bad c = ord c `elem` [0x2028, 0x2029]
