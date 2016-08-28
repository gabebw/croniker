module Handler.CreateMonikerSpec
    ( main
    , spec
    ) where

import TestImport

import Data.Time.Zones.All (TZLabel(..))
import Network.Wai.Test (simpleBody, simpleHeaders)
import qualified Data.Text as T

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ do
    describe "Creating a moniker" $ do
        describe "when signed in" $ do
            it "strips whitespace from the moniker when it is invalid" $ do
                let user = buildUser
                let strippedMoniker = "twitter"
                let moniker = " " <> strippedMoniker <> "\t"
                void $ runDB $ insert user
                loginAs user

                get RootR
                void followRedirect
                postForm ProfileR $ do
                    byLabel "Moniker" moniker
                    byLabel "Date" "3000-01-01"
                void followRedirect

                let query = "input[value=" <> strippedMoniker <> "]"
                htmlCount query 1

buildUser :: User
buildUser = User "1" "gabebw" "token123" "secret123" Etc__UTC True True

debugResponse :: forall site. YesodExample site ()
debugResponse = withResponse $ \response -> do
    print $ simpleHeaders response
    print $ simpleBody response
