module Handler.RootSpec
    ( main
    , spec
    ) where

import TestImport

import Data.Time.Zones.All (TZLabel(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ do
    describe "Visiting the root path" $ do
        describe "when signed out" $ do
            it "shows a welcome message" $ do
                get RootR

                statusIs 200
                bodyContains "Schedule changes to your Twitter name with Croniker"

        describe "when signed in" $ do
            it "shows the profile form" $ do
                let user = buildUser
                void $ runDB $ insert user
                loginAs user

                get RootR
                void $ followRedirect

                bodyContains "gabebw"
                bodyContains "Defaults to the next available date"

buildUser :: User
buildUser = User "1" "gabebw" "token123" "secret123" Etc__UTC True
