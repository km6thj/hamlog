{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Ham.Internal.Log.V2
  where

import Ham.Internal.Qso
import Ham.Qso
import qualified Ham.CAT as CAT
import Data.Text
import Data.Time.Calendar
import Data.Time.Clock
import Data.Sequence

import Data.Aeson
import Data.Aeson.TH

import Lens.Micro.TH


-- | Configuration for a log, version 2.
data LogConfigV2 = LogConfigV2 {
  _configLogFile :: FilePath          -- ^ File to store the logged Qsos in.
  , _configQsoDefaults :: QsoDefaults -- ^ Default values, if any, for Qsos.
  , _configCat :: CAT.CATConfig       -- ^ CAT configuration.
  } deriving Show


$(deriveJSON defaultOptions ''LogConfigV2)

makeLenses ''LogConfigV2
