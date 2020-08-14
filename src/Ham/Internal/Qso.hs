{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Ham.Internal.Qso
  where

import Data.Char (isDigit, digitToInt)
import Data.List (all)
import Data.Text
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

import Data.Aeson
import Data.Aeson.TH

import Lens.Micro.TH

import Ham.Internal.Data


-- | Contains information about a ham radio contact.
data Qso = Qso {
      _qsoTimeStart    :: UTCTime
    , _qsoTimeEnd      :: UTCTime
    , _qsoFrequency    :: Frequency
    , _qsoMode         :: QsoMode
    , _qsoCallsign     :: Text
    , _qsoLocation     :: Text
    , _qsoRST          :: RST
    , _qsoExchange     :: Text
    , _qsoSentCallsign :: Text
    , _qsoSentLocation :: Text
    , _qsoSentRST      :: RST
    , _qsoSentExchange :: Text
    , _qsoNotes        :: Text
    } deriving (Show, Eq)



-- | Sanitizing a Qso.
-- Strips leading and trailing whitespace and converts to upper case.
sanitizeQso :: Qso -> Qso
sanitizeQso q = q { _qsoCallsign = toUpper $ strip $ _qsoCallsign q }


data DefaultValue a = FixedValue a | DefaultValue a deriving (Show, Read)
defaultValueGet :: DefaultValue a -> a
defaultValueGet (FixedValue a) = a
defaultValueGet (DefaultValue a) = a

--instance Show a => Show (DefaultValue a)
--instance Read a => Read (DefaultValue a)

-- | Default values to fill in to a 'Qso'.
-- These are kept in the configuration.
-- For each entry in 'Qso', there is one default that either marks one
-- as fixed value, or changeable value.
data QsoDefaults = QsoDefaults {
      _qsoDefaultTimeStart    :: DefaultValue UTCTime
    , _qsoDefaultTimeEnd      :: DefaultValue UTCTime
    , _qsoDefaultFrequency    :: DefaultValue Frequency
    , _qsoDefaultMode         :: DefaultValue QsoMode
    , _qsoDefaultCallsign     :: DefaultValue Text
    , _qsoDefaultLocation     :: DefaultValue Text
    , _qsoDefaultRST          :: DefaultValue RST
    , _qsoDefaultExchange     :: DefaultValue Text
    , _qsoDefaultSentCallsign :: DefaultValue Text
    , _qsoDefaultSentLocation :: DefaultValue Text
    , _qsoDefaultSentRST      :: DefaultValue RST
    , _qsoDefaultSentExchange :: DefaultValue Text
    , _qsoDefaultNotes        :: DefaultValue Text
    } deriving (Show, Read)


makeLenses ''Qso


$(deriveJSON defaultOptions ''Qso)
$(deriveJSON defaultOptions ''DefaultValue)
$(deriveJSON defaultOptions ''QsoDefaults)
