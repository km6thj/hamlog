{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Ham.Internal.Log
  where

import Ham.Internal.Qso
import Ham.Qso
import Data.Text
import Data.Time.Calendar
import Data.Time.Clock
import Data.Sequence

import Data.Aeson
import Data.Aeson.TH

import Lens.Micro.TH


-- | A log.
-- A log contains a set of 'Qso's
data Log = Log {
  _logQsos :: Seq Qso  -- ^ The set of QSOs contained in this log.
  } deriving Show

-- | Configuration for a log.
data LogConfig = LogConfig {
  _configLogFile :: FilePath       -- ^ File to store the logged Qsos in.
  , _configQsoDefaults :: QsoDefaults -- ^ Default values, if any, for Qsos.
  } deriving Show

$(deriveJSON defaultOptions ''Log)
$(deriveJSON defaultOptions ''LogConfig)

makeLenses ''Log
