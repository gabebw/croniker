module Handler.ProfileSpec
    ( main
    , spec
    ) where

import TestImport

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ do
    describe "Visiting the profile path" $ do
        describe "when signed out" $ do
            it "redirects to / (eventually)" $ do
                get ProfileR

                assertHeader "Location" "/auth/login"
                void followRedirect
                assertHeader "Location" "/"
                void followRedirect

                assertNoHeader "Location"
