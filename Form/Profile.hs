module Form.Profile
    ( profileForm
    ) where

import Import

import qualified Croniker.MonikerNormalization as CMN
import qualified Croniker.MonikerFieldChecks as MonikerFieldChecks (runAllChecks)
import Model.FormProfile (FormProfile(..))

profileForm :: Day -> [Day] -> Day -> Form FormProfile
profileForm nextFreeDay takenDays tomorrow = renderDivs $ FormProfile
    <$> fmap normalizeMaybe
            (aopt
                monikerField
                (fs "Moniker" [("maxlength", "20"), ("autofocus", "autofocus")])
                Nothing)
    <*> aopt
            textareaField
            (fs "Bio" [("maxlength", "160"), ("rows", "4")])
            Nothing
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
monikerField = MonikerFieldChecks.runAllChecks textField

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
