{-# LANGUAGE OverloadedStrings #-}

module Ham.ADIF (qsoAdif) where

import Ham.Internal.Data
import Ham.Internal.Qso
import Data.List (intersperse)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Format
import Data.Time

-- FIXME: Check the ADIF format of the LOTW ADIF files for upload there.
-- Support only the minimum for the LOTW export for a start.
-- Move the Cabrillo and ADIF code into an Export module, possibly.


adifLine :: ADIFText a => Text -> a -> Text
adifLine name value = "<" <> name <> ":" <> T.pack (show $ T.length v) <> ">" <> v
  where v = adifText value


class ADIFText a where
  adifText :: a -> Text

instance ADIFText Text where
  adifText = id


newtype ADIFDate = ADIFDate UTCTime
newtype ADIFTime = ADIFTime UTCTime


instance ADIFText ADIFDate where
  adifText (ADIFDate t) = T.pack $ formatTime defaultTimeLocale "%Y%m%d" t


instance ADIFText ADIFTime where
  adifText (ADIFTime t) = T.pack $ formatTime defaultTimeLocale "%H%M%S" t


instance ADIFText Band where
  adifText (M f) = T.pack $ band_name f
  adifText Microwave = "Microwave"


instance ADIFText QsoMode where
  adifText md = case md of
    CW -> "CW"
    PH -> "SSB"
    FM -> "FM"
    RY -> "RTTY"
    DATA -> "PSK" -- FIXME: This shows some insufficiency of the QsoMode data type. Make it support all and custom modes. For my LOTW efforts this should be fine.


qsoAdif :: Qso -> Text
qsoAdif qso = T.concat $ intersperse "\n" lines
  where lines = [call, bd, md, qso_date, time_on, "<EOR>"]
        call = adifLine "CALL" $ _qsoCallsign qso
        bd = adifLine "BAND" $ band $ _qsoFrequency qso
        md = adifLine "MODE" $ _qsoMode qso
        qso_date = adifLine "QSO_DATE" $ ADIFDate $ _qsoTimeStart qso
        time_on = adifLine "TIME_ON" $ ADIFTime $ _qsoTimeStart qso
