module Croniker.MonikerNormalizationSpec
    ( main
    , spec
    ) where

import Prelude
import Test.Hspec

import Control.Monad (forM_)
import Croniker.MonikerNormalization (normalize)
import Data.Char (chr)
import Text.Printf (printf)
import qualified Data.Text as T

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Croniker.MonikerNormalization" $ do
    describe "normalize" $ do
        forM_ [0x2028, 0x2029] $ \c -> do
            it (printf "strips U+%X from the beginning of the moniker" c) $ do
                let t = T.pack $ chr c:"hello"
                normalize t `shouldBe` "hello"

            it (printf "strips U+%X from the end of the moniker" c) $ do
                let t = T.pack $ "hello" ++ [chr c]
                normalize t `shouldBe` "hello"

            it (printf "does not strip U+%X from the middle of the moniker" c) $ do
                let t = T.pack $ concat ["he", [chr c], "llo"]
                let good = T.concat ["he", T.singleton $ chr c, "llo"]
                normalize t `shouldBe` good

        forM_ [0x202A..0x202F] $ \c -> do
            it (printf "strips U+%X from the beginning of the moniker" c) $ do
                let t = T.pack $ chr c:"hello"
                normalize t `shouldBe` "hello"

            it (printf "strips U+%X from the end of the moniker" c) $ do
                let t = T.pack $ "hello" ++ [chr c]
                normalize t `shouldBe` "hello"

            it (printf "strips U+%X from the middle of the moniker" c) $ do
                let t = T.pack $ concat ["he", [chr c], "llo"]
                normalize t `shouldBe` "hello"

        it "strips chars above U+FEFF from the moniker" $ do
            let astral_plane_char = T.singleton $ chr 0x10000

            let t = T.concat [
                        astral_plane_char,
                        "he",
                        astral_plane_char,
                        "llo",
                        astral_plane_char
                    ]
            normalize t `shouldBe` "hello"

        it "removes leading whitespace from the moniker" $ do
            let t = "\t\n  hello"
            normalize t `shouldBe` "hello"

        it "removes trailing whitespace from the moniker" $ do
            let t = "hello\t\n  "
            normalize t `shouldBe` "hello"
