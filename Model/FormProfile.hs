module Model.FormProfile
    ( addProfile
    , valuesArePresent
    , buildFormProfile
    , FormProfile
    ) where

import Import

import qualified Croniker.MonikerNormalization as CMN
import Data.Conduit.Binary (sinkLbs)
import Helper.TextConversion (base64Encode)

data FormProfile = FormProfile
    { profileMoniker :: Maybe Text
    , profileDescription :: Maybe Textarea
    , profilePicture :: Maybe FileInfo
    , profileDate :: Day
    }

buildFormProfile :: Maybe Text -> Maybe Textarea -> Maybe FileInfo -> Day -> FormProfile
buildFormProfile moniker desc picture date = FormProfile
    (CMN.normalize <$> moniker)
    desc
    picture
    date

addProfile :: UserId -> FormProfile -> Handler ()
addProfile userId (FormProfile moniker description picture date) = do
    base64Picture <- base64Bytes picture
    void $ runDB $ insert $ Profile
        moniker
        date
        userId
        base64Picture
        (unTextarea <$> description)
        False

valuesArePresent :: FormProfile -> Bool
valuesArePresent FormProfile{profileMoniker, profilePicture, profileDescription} =
    isJust profileMoniker ||
        isJust profilePicture ||
        isJust profileDescription

-- If a picture was uploaded, base64-encode it.
base64Bytes :: Maybe FileInfo -> Handler (Maybe Text)
base64Bytes Nothing = return Nothing
base64Bytes (Just fi) = do
    fileBytes <- runResourceT $ fileSource fi $$ sinkLbs
    return $ Just $ base64Encode fileBytes
