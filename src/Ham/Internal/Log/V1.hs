{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Ham.Internal.Log.V1
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



-- | Configuration for a log, version 1.
data LogConfigV1 = LogConfigV1 {
  _configLogFile :: FilePath       -- ^ File to store the logged Qsos in.
  , _configQsoDefaults :: QsoDefaults -- ^ Default values, if any, for Qsos.
  } deriving Show



$(deriveJSON defaultOptions ''LogConfigV1)
