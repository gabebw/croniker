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
    , profileDate :: Day
    , profilePicture :: Maybe FileInfo
    }

addProfile :: UserId -> FormProfile -> Handler ()
addProfile userId (FormProfile moniker date picture) = do
    base64Picture <- base64Bytes picture
    void $ runDB $ insert $ Profile moniker date userId base64Picture False

valuesArePresent :: FormProfile -> Bool
valuesArePresent FormProfile{profileMoniker, profilePicture} =
    isJust profileMoniker ||
        isJust profilePicture

-- If a picture was uploaded, base64-encode it.
base64Bytes :: Maybe FileInfo -> Handler (Maybe Text)
base64Bytes Nothing = return Nothing
base64Bytes (Just fi) = do
    fileBytes <- runResourceT $ fileSource fi $$ sinkLbs
    return $ Just $ base64Encode fileBytes
