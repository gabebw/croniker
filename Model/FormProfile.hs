module Model.FormProfile
    ( addProfile
    , valuesArePresent
    , FormProfile(..)
    ) where

import Import

import Data.Conduit.Binary (sinkLbs)
import Helper.TextConversion (base64Encode)

data FormProfile = FormProfile
    { profileMoniker :: Maybe Text
    , profileDescription :: Maybe Textarea
    , profilePicture :: Maybe FileInfo
    , profileDate :: Day
    }

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
    fileBytes <- runConduit $ fileSource fi .| sinkLbs
    return $ Just $ base64Encode fileBytes
