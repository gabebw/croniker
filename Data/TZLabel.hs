{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.TZLabel where

import ClassyPrelude.Yesod
import Database.Persist ()
import Database.Persist.Sql
import Data.Time.Zones.All (TZLabel, fromTZName, toTZName)
import Helper.TextConversion
import qualified Data.Text as T (pack)

-- (De)serialize a TZLabel as a Text field in the database
instance PersistField TZLabel where
    toPersistValue tzLabel = PersistText $ b2t $ toTZName tzLabel
    fromPersistValue (PersistText text) =
        case fromTZName (t2b text) of
            (Just tzLabel) -> Right tzLabel
            Nothing -> Left ("Bad TZ name: " <> text)
    fromPersistValue bad = Left ("Bad TZ name: " <> (T.pack (show bad)))

instance PersistFieldSql TZLabel where
    sqlType _ = SqlString
