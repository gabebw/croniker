module Handler.FeedSpec
    ( main
    , spec
    ) where

import TestImport
import Data.Time.Zones.All (TZLabel(..))
import qualified Croniker.Time as CT

main :: IO ()
main = hspec spec

buildPublicUser :: User
buildPublicUser = User "1" "gabebw" "token123" "secret123" Etc__UTC True True

buildPrivateUser :: User
buildPrivateUser = buildPublicUser
    { userTwitterUserId = "2"
    , userTwitterUsername = "gabebw_ebooks"
    , userPublicRssFeed = False }

buildProfile :: UserId -> Day -> Profile
buildProfile userId day = Profile (Just "newname") day userId (Just "newpicture") (Just "newdescription") True

spec :: Spec
spec = withApp $ do
    describe "Visiting a public feed" $ do
        describe "when signed out" $ do
            it "works" $ do
                userId <- runInsert buildPublicUser

                get $ FeedR userId

                statusIs 200

            it "only contains profile information for past profiles" $ do
                userId <- runInsert buildPublicUser
                yesterday <- CT.yesterday
                today <- CT.today
                tomorrow <- CT.tomorrow
                runInsert_ $ (buildProfile userId yesterday) { profileMoniker = Just "past moniker" }
                runInsert_ $ (buildProfile userId today) { profileMoniker = Just "today moniker" }
                runInsert_ $ (buildProfile userId tomorrow) { profileMoniker = Just "future moniker" }

                get $ FeedR userId

                htmlCount "item" 1
                htmlAllContain "item description" "Changed moniker to past moniker"

            it "contains profile information" $ do
                userId <- runInsert buildPublicUser
                day <- CT.yesterday
                void $ runInsert $ Profile (Just "newname") day userId (Just "newpicture") (Just "newdescription") True

                get $ FeedR userId

                htmlCount "item" 1
                htmlAnyContain "description" "Past Twitter names for gabebw"
                htmlAnyContain "description" "Changed moniker to newname"
                htmlAnyContain "title" "Changed moniker, bio and picture"

    describe "Visiting a private feed" $ do
        describe "when signed out" $ do
            it "requires the user to log in" $ do
                userId <- runInsert buildPrivateUser

                get $ FeedR userId

                statusIs 303
                assertHeader "Location" "/auth/login"

        describe "when signed in as that user" $ do
            it "displays the feed" $ do
                let user = buildPrivateUser
                userId <- runInsert user
                loginAs user

                get $ FeedR userId
                assertNoHeader "Location"

                statusIs 200
                htmlAnyContain "description" "Past Twitter names for gabebw"

        describe "when signed in as another user" $ do
            it "404s" $ do
                let user = buildPrivateUser
                let otherUser = buildPublicUser
                userId <- runInsert user
                runInsert_ otherUser
                loginAs otherUser

                get $ FeedR userId
                assertNoHeader "Location"

                statusIs 404
