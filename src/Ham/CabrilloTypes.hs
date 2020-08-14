{-# LANGUAGE OverloadedStrings #-}

module Ham.CabrilloTypes where

import Ham.Cabrillo
import Data.Text (pack)

naqp :: Callsign  -- ^ My callsign
     -> Location  -- ^ My location
     -> String    -- ^ My name
     -> Cabrillo
naqp cs loc nm = defaultCabrillo { contest = Just "NAQP-SSB"
                               ,location = Just $ pack loc
                               ,callsign = Just $ pack cs
                               ,category_operator = Just SINGLE_OP
                               ,category_transmitter = Just ONE
                               ,category_assisted = Just NonAssisted
                               ,category_band = Just "ALL"
                               ,category_power = Just LOW
                               ,category_mode = Just CMSSB
                               ,category_station = Just FIXED
                               ,claimed_score = Just 0
                               ,name = Just $ pack nm}


field_day :: Callsign  -- ^ My callsign
          -> Location  -- ^ My location
          -> String    -- ^ My name
          -> Cabrillo
field_day cs loc nm = defaultCabrillo { contest = Just "ARRL-FIELDDAY"
                                      ,location = Just $ pack loc
                                      ,callsign = Just $ pack cs
                                      ,category_operator = Just SINGLE_OP
                                      ,category_transmitter = Just ONE
                                      ,category_assisted = Just NonAssisted
                                      ,category_band = Just "ALL"
                                      ,category_power = Just LOW
                                      ,category_mode = Just CMMIXED
                                      ,category_station = Just FIXED
                                      ,claimed_score = Just 0
                                      ,name = Just $ pack nm}
