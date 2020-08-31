{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Ham.Internal.Log.Log
  where

import Ham.Internal.Qso
import Ham.Qso
import Data.Sequence

import Data.Aeson
import Data.Aeson.TH

import Lens.Micro.TH


-- | A log.
-- A log contains a set of 'Qso's
data Log = Log {
  _logQsos :: Seq Qso  -- ^ The set of QSOs contained in this log.
  } deriving Show


$(deriveJSON defaultOptions ''Log)

makeLenses ''Log
