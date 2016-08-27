module Handler.CreateMonikerSpec
    ( main
    , spec
    ) where

import TestImport

import Data.Time.Zones.All (TZLabel(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ do
    describe "Creating a moniker" $ do
        describe "when signed in" $ do
            it "strips whitespace from the moniker" $ do
                let user = buildUser
                void $ runDB $ insert user
                loginAs user

                get RootR
                void followRedirect
                postForm ProfileR $ do
                    byLabel "Moniker" " twitter "

                htmlCount "input[id=hident2][value= twitter ]" 1

buildUser :: User
buildUser = User "1" "gabebw" "token123" "secret123" Etc__UTC True True
