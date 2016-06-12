module Croniker.UrlParserSpec
    ( main
    , spec
    ) where

import Prelude
import Test.Hspec

import Croniker.UrlParser (containsUrl)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Croniker.UrlParser" $ do
    describe "containsUrl" $ do
        it "returns true for strings that contain URLs" $ do
            containsUrl "hello from something.com" `shouldBe` True

        it "returns true for strings that contain mixed-case URLs" $ do
            containsUrl "hello from SOMEthing.CoM" `shouldBe` True

        it "returns true for strings that contain fancy URLs" $ do
            containsUrl "hello from something.club" `shouldBe` True

        it "returns true for strings that contain fancier URLs" $ do
            containsUrl "hello from something.co.uk" `shouldBe` True

        it "returns true for strings that are only URLs" $ do
            containsUrl "something.com" `shouldBe` True

        it "returns true for strings with http://-prefixed URLs" $ do
            containsUrl "http://something.com" `shouldBe` True

        it "returns true for strings with https://-prefixed URLs" $ do
            containsUrl "https://something.com" `shouldBe` True

        it "returns false for strings with nothing that looks like a URL" $ do
            containsUrl "hello" `shouldBe` False

        it "returns false for strings that have only a TLD" $ do
            containsUrl "com" `shouldBe` False

        it "returns false for strings that have only a dot-TLD" $ do
            containsUrl ".com" `shouldBe` False

        it "returns false for strings that contain a file extension" $ do
            containsUrl "hello.tar.gz" `shouldBe` False

        it "returns false for non-URLs that start with 'http'" $ do
            containsUrl "httphi" `shouldBe` False
