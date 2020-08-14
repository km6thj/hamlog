{-# LANGUAGE OverloadedStrings #-}

module Ham.Internal.Fcc where

import Data.Aeson


-- | Result received from a FCC database query.
data FccResult = FccResult
  {
    status :: String         -- ^ Status code of the licenses.
  , licenses :: FccLicenses  -- ^ The actual licenses.
   } deriving (Read, Show)

data FccLicenses = FccLicenses
  { lastUpdate :: String
  , page :: String
  , totalRows :: String
  , license :: [FccLicense]
  , rowPerPage :: String
  } deriving (Read, Show)

data FccLicense = FccLicense
  { frn :: String
  , licName :: String
  , serviceDesc :: String
  , licDetailURL :: String
  , statusDesc :: String
  , expiredDate :: String
  , licenseID :: String
  , callsign :: String
  , categoryDesc :: String } deriving (Read, Show)

resultLabelMod :: String -> String
resultLabelMod "licenses" = "Licenses"
resultLabelMod "license" = "License"
resultLabelMod a = a

resultConstructorMod :: String -> String
resultConstructorMod "licenses" = "Licenses"
resultConstructorMod "license" = "License"
resultConstructorMod a = a

