module Form.Profile
    ( profileForm
    ) where

import Import

import qualified Croniker.MonikerNormalization as CMN
import qualified Croniker.UrlParser as CUP
import Model.FormProfile (FormProfile(..))

profileForm :: Day -> [Day] -> Day -> Form FormProfile
profileForm nextFreeDay takenDays tomorrow = renderDivs $ FormProfile
    <$> fmap normalizeMaybe
            (aopt
                monikerField
                (fs "New moniker" [("maxlength", "20"), ("autofocus", "autofocus")])
                Nothing)
    <*> (aopt
            textField
            (fs "Description" [("maxlength", "160")])
            Nothing)
    <*> aopt fileField "Profile picture" Nothing
    <*> areq
            (dateField takenDays tomorrow)
            ("Date" { fsTooltip = Just "Defaults to the next available date" })
            (Just nextFreeDay)
    where
        normalizeMaybe = fmap CMN.normalize

fs :: Text -> [(Text, Text)] -> FieldSettings site
fs label attrs = FieldSettings
    { fsLabel = SomeMessage label
    , fsTooltip = Nothing
    , fsId = Nothing
    , fsName = Nothing
    , fsAttrs = attrs
    }

monikerField :: Field Handler Text
monikerField = foldr check textField [
                   doesNotContainUrl,
                   doesNotContainTwitter,
                   validWhitespace . CMN.normalize,
                   validLength . CMN.normalize
               ]

doesNotContainTwitter :: Text -> Either Text Text
doesNotContainTwitter moniker
    | "twitter" `isInfixOf` toLower moniker = Left "Twitter doesn't allow monikers that contain \"Twitter\""
    | otherwise = Right moniker

doesNotContainUrl :: Text -> Either Text Text
doesNotContainUrl moniker
    | CUP.containsUrl moniker = Left "Twitter doesn't allow URLs in monikers"
    | otherwise = Right moniker

validLength :: Text -> Either Text Text
validLength moniker
    | length moniker == 0 = Left "Monikers cannot be blank"
    | length moniker > 20 = Left "Twitter doesn't allow monikers longer than 20 characters"
    | otherwise = Right moniker

validWhitespace :: Text -> Either Text Text
validWhitespace moniker
    | any (`isInfixOf` moniker) ["\n", "\t"] = Left "Moniker cannot contain special whitespace characters"
    | otherwise = Right moniker

dateField :: [Day] -> Day -> Field Handler Day
dateField takenDays tomorrow = check (nothingScheduled takenDays) $ check (futureDate tomorrow) dayField

futureDate :: Day -> Day -> Either Text Day
futureDate tomorrow date
    | date < tomorrow = Left "You must select a future date"
    | otherwise = Right date

nothingScheduled :: [Day] -> Day -> Either Text Day
nothingScheduled takenDays date
    | date `elem` takenDays = Left "You already have a change scheduled for that day"
    | otherwise = Right date
