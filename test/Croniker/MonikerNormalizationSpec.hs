module Croniker.MonikerNormalizationSpec
    ( main
    , spec
    ) where

import ClassyPrelude
import Test.Hspec

import Croniker.MonikerFieldChecks
import Data.Char (chr)
import Data.Either (isLeft)
import Text.Printf (printf)

main :: IO ()
main = hspec spec

-- Helpful for generating Unicode codepoints like U+267B.
-- `codepoint2text 0x267B` returns a Text that contains U+267B.
codepoint2text :: Int -> Text
codepoint2text codepoint = singleton $ chr codepoint

checker :: [Text -> Either Text Text] -> Text -> Either Text Text
checker [] t = Right t
checker (c:cs) t = if isLeft (c t) then c t else checker cs t

checkAll :: Text -> Either Text Text
checkAll = checker allChecks

spec :: Spec
spec = describe "Croniker.MonikerFieldChecks" $ do
    describe "running all checks" $ do
        it "does not warn about a valid moniker" $ do
            let t = "hello there!"

            checkAll t `shouldBe` Right t

        it "allows codepoints below U+FFFF" $ do
            let recyclingSymbol = codepoint2text 0x267B
            let t = recyclingSymbol `mappend` "hello"

            checkAll t `shouldBe` Right t

        it "allows codepoints above U+FFFF" $ do
            let astral_plane_char = codepoint2text 0x10000

            let t = concat [
                        astral_plane_char,
                        "he",
                        astral_plane_char,
                        "llo",
                        astral_plane_char
                    ]

            checkAll t `shouldBe` Right t

        forM_ [0x2028, 0x2029] $ \c -> do
            it (printf "warns about U+%X at the beginning of the moniker" c) $ do
                let t = codepoint2text c `mappend` "hello"

                checkAll t `shouldBe` Left "Moniker can't start or end with U+2028 or U+2029"

            it (printf "warns about U+%X at the end of the moniker" c) $ do
                let t = "hello" `mappend` codepoint2text c

                checkAll t `shouldBe` Left "Moniker can't start or end with U+2028 or U+2029"

            it (printf "does not warn about U+%X in the middle of the moniker" c) $ do
                let t = concat ["he", codepoint2text c, "llo"]

                checkAll t `shouldBe` Right t

        forM_ [0x202A..0x202F] $ \c -> do
            it (printf "warns about U+%X at the beginning of the moniker" c) $ do
                let t = codepoint2text c `mappend` "hello"

                checkAll t `shouldBe` Left "Moniker can't contain special whitespace characters"

            it (printf "warns about U+%X at the end of the moniker" c) $ do
                let t = "hello" `mappend` codepoint2text c

                checkAll t `shouldBe` Left "Moniker can't contain special whitespace characters"

            it (printf "warns about U+%X in the middle of the moniker" c) $ do
                let t = concat ["he", codepoint2text c, "llo"]

                checkAll t `shouldBe` Left "Moniker can't contain special whitespace characters"

        it "removes leading whitespace from the moniker before checking" $ do
            let exactlyLongEnough = "01234567890123456789"
            let t = " \t\n" `T.append` exactlyLongEnough

            normalize t `shouldBe` Right exactlyLongEnough

        it "removes trailing whitespace from the moniker before checking" $ do
            let exactlyLongEnough = "01234567890123456789"
            let t = exactlyLongEnough `T.append` " \t\n"

            normalize t `shouldBe` Right exactlyLongEnough

        it "warns about all opening angle brackets" $ do
            let t = "<h<e<l<l<o<"

            checkAll t `shouldBe` Left "Moniker cannot contain \"<\" or \">\""

        it "warns about all closing angle brackets" $ do
            let t = ">h>e>l>l>o>"

            checkAll t `shouldBe` Left "Moniker cannot contain \"<\" or \">\""
