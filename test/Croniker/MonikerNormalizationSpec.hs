module Croniker.MonikerNormalizationSpec
    ( main
    , spec
    ) where

import ClassyPrelude
import Test.Hspec

-- import Control.Monad (forM_)
import Croniker.MonikerNormalization (normalize)
import Data.Char (chr)
import Text.Printf (printf)

main :: IO ()
main = hspec spec

-- Helpful for stuff like U+267B.
-- `codepoint2text 0x267B` returns a Text that contains U+267B.
codepoint2text :: Int -> Text
codepoint2text codepoint = singleton $ chr codepoint

spec :: Spec
spec = describe "Croniker.MonikerNormalization" $ do
    describe "normalize" $ do
        it "allows codepoints below U+FFFF" $ do
            let recyclingSymbol = codepoint2text 0x267B
            let t = recyclingSymbol `mappend` "hello"

            normalize t `shouldBe` t

        it "allows codepoints above U+FFFF" $ do
            let astral_plane_char = codepoint2text 0x10000

            let t = concat [
                        astral_plane_char,
                        "he",
                        astral_plane_char,
                        "llo",
                        astral_plane_char
                    ]
            normalize t `shouldBe` t

        forM_ [0x2028, 0x2029] $ \c -> do
            it (printf "strips U+%X from the beginning of the moniker" c) $ do
                let t = codepoint2text c `mappend` "hello"
                normalize t `shouldBe` "hello"

            it (printf "strips U+%X from the end of the moniker" c) $ do
                let t = "hello" `mappend` codepoint2text c
                normalize t `shouldBe` "hello"

            it (printf "does not strip U+%X from the middle of the moniker" c) $ do
                let t = concat ["he", codepoint2text c, "llo"]
                normalize t `shouldBe` t

        forM_ [0x202A..0x202F] $ \c -> do
            it (printf "strips U+%X from the beginning of the moniker" c) $ do
                let t = codepoint2text c `mappend` "hello"
                normalize t `shouldBe` "hello"

            it (printf "strips U+%X from the end of the moniker" c) $ do
                let t = "hello" `mappend` codepoint2text c
                normalize t `shouldBe` "hello"

            it (printf "strips U+%X from the middle of the moniker" c) $ do
                let t = concat ["he", codepoint2text c, "llo"]
                normalize t `shouldBe` "hello"

        it "removes leading whitespace from the moniker" $ do
            let t = "\t\n  hello"
            normalize t `shouldBe` "hello"

        it "removes trailing whitespace from the moniker" $ do
            let t = "hello\t\n  "
            normalize t `shouldBe` "hello"

        it "removes all opening angle brackets" $ do
            let t = "<h<e<l<l<o<"

            normalize t `shouldBe` "hello"

        it "removes all closing angle brackets" $ do
            let t = ">h>e>l>l>o>"

            normalize t `shouldBe` "hello"
