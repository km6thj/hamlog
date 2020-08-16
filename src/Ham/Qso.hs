{-# LANGUAGE OverloadedStrings #-}

module Ham.Qso
  (
    QsoMode(..),
    Qso(..),
    QsoDefaults(..), DefaultValue(..),
    emptyQso, qsoWithDefaults, emptyQsoDefaults, Frequency(..),
    sanitizeQso,
    qsoTimeStart,
    qsoTimeEnd,
    qsoFrequency,
    qsoMode,
    qsoCallsign,
    qsoLocation,
    qsoRST,
    qsoExchange,
    qsoSentCallsign,
    qsoSentLocation,
    qsoSentRST,
    qsoSentExchange,
    qsoNotes,
    module Data.Time.Clock,
    module Data.Time.Calendar,
    module Data.Time.LocalTime
  ) where


import Ham.Internal.Qso
import qualified Ham.Cabrillo as C
import Ham.Cabrillo (ToCabrillo)
import Ham.Data as D
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Text (unpack)



defaultQsoTime = UTCTime (fromGregorian 2018 01 22) (timeOfDayToTime $ TimeOfDay 00 00 00)
emptyQso = qsoWithDefaults emptyQsoDefaults -- Qso defaultQsoTime defaultQsoTime (MHz 0) PH mempty mempty (RST 0 0 0) mempty mempty mempty (RST 0 0 0) mempty mempty

emptyQsoDefaults = QsoDefaults (DefaultValue defaultQsoTime) (DefaultValue defaultQsoTime) (DefaultValue (MHz 0)) (DefaultValue PH) (DefaultValue mempty) (DefaultValue mempty) (DefaultValue (RST 0 0 0)) (DefaultValue mempty) (DefaultValue mempty) (DefaultValue mempty) (DefaultValue (RST 0 0 0)) (DefaultValue mempty) (DefaultValue mempty)

qsoWithDefaults :: QsoDefaults -> Qso
qsoWithDefaults d = q
  where q = Qso {
                  _qsoTimeStart    = f _qsoDefaultTimeStart
                , _qsoTimeEnd      = f _qsoDefaultTimeEnd
                , _qsoFrequency    = f _qsoDefaultFrequency
                , _qsoMode         = f _qsoDefaultMode
                , _qsoCallsign     = f _qsoDefaultCallsign
                , _qsoLocation     = f _qsoDefaultLocation
                , _qsoRST          = f _qsoDefaultRST
                , _qsoExchange     = f _qsoDefaultExchange
                , _qsoSentCallsign = f _qsoDefaultSentCallsign
                , _qsoSentLocation = f _qsoDefaultSentLocation
                , _qsoSentRST      = f _qsoDefaultSentRST
                , _qsoSentExchange = f _qsoDefaultSentExchange
                , _qsoNotes        = f _qsoDefaultNotes
                }
        f a = defaultValueGet $ a d


instance ToCabrillo Qso where
    toCabrillo q = [C.CLQso $ C.CabrilloQso (_qsoFrequency q)
                                (modeConvert (_qsoMode q))
                                (_qsoTimeStart q)
                                s
                                r
                                Nothing]
            where s = C.CabrilloInfo (unpack $ _qsoSentCallsign q) (unpack $ _qsoSentExchange q)
                  r = C.CabrilloInfo (unpack $ _qsoCallsign q) (unpack $ _qsoExchange q)

modeConvert :: QsoMode -> C.Mode
modeConvert D.CW = C.CW
modeConvert D.PH = C.PH
modeConvert D.FM = C.FM
modeConvert D.RY = C.RY
modeConvert D.DATA = C.DG
