module Handler.RootSpec
    ( main
    , spec
    ) where

import TestImport

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
