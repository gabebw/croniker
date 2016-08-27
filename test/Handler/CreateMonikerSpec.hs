module Handler.CreateMonikerSpec
    ( main
    , spec
    ) where

import TestImport

import Network.Wai.Test (simpleBody)
import Text.XML.Cursor hiding (element)

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

                withResponse $ \response -> do
                    let body = simpleBody response
                    htmlQuery "input[id=hident2][value= twitter ]"

                    -- let input = parseHTML body $// attributeIs "id" "hident2" >=> attribute "value"
                    -- let input = " twitter "
                    -- shouldBe input " twitter "
                    -- htmlCount "input#hident2" 1
                    -- items `shouldHaveLength` 0
                    -- bodyContains "gabebw"
                    -- htmlAnyContain "input" "Site 1"
                    -- bodyContains "Defaults to the next available date"

buildUser :: User
buildUser = User "1" "gabebw" "token123" "secret123" Etc__UTC True True
