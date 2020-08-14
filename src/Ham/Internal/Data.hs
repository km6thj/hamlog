{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Ham.Internal.Data where

import Data.Aeson
import Data.Aeson.TH
import Data.List
import Data.Char (isDigit, digitToInt)

-- | Frequency in MHz.
newtype Frequency = MHz Float
                  deriving (Show, Read, Eq, Ord)


-- | Wavelength in metres.
wavelength :: Frequency -> Float
wavelength (MHz f) = c / (f * 1e6)
  where c = 299792458 -- m / s

-- | Commmon bands in meters.
bands = [160, 80, 60, 40, 30, 20, 17, 15, 12, 10, 6, 2, 1.25, 0.7, 0.33, 0.23]

band_name :: Float -> String
band_name a | (fromIntegral $ round a) == a = show (round a) <> "m"
            | a > 1.0 = show a <> "m"
            | otherwise = show (round $ a * 100) <> "cm"

data Band = M Float     -- ^ Band in metres
          | Microwave   -- ^ Anything 15% shorter than about 23cm.
          deriving Eq

instance Show Band where
  show (M b) = band_name b
  show Microwave = "Microwave"


-- | Find canonical band name for frequency.
band :: Frequency -> Band
band f = case m of
           (0.23, d) -> if (d / 0.23) > 0.15 then Microwave else M 0.23
           (a, _) -> M a
  where l = wavelength f
        diffs = map (\a -> (a, abs (l - a))) bands
        m = minimumBy (\a b -> compare (snd a) (snd b)) diffs



-- | Modes for a QSO.
-- TODO: This is following Cabrillo; this should go into the Cabrillo module and this type here should have many more entries.
data QsoMode = CW  -- ^ Morse code.
             | PH  -- ^ Voice.
             | FM  -- ^ FM voice.
             | RY  -- ^ RTTY or other teletype mode.
             | DATA
             deriving (Show, Read, Eq)


-- | RST report.
data RST = RST Int Int Int  -- ^ Readability, signal strength, tone.
         | RS Int Int       -- ^ Readability, signal strength
         | NoRST deriving (Eq, Ord)



rstReadability :: RST -> Int
rstReadability (RST r _ _) = r
rstReadability (RS r _)    = r
rstReadability _           = 0

rstStrength :: RST -> Int
rstStrength (RST _ s _) = s
rstStrength (RS _ s)    = s
rstStrength _           = 0

rstTone :: RST -> Int
rstTone (RST _ _ t) = t
rstTone (RS _ _)    = 0
rstTone _           = 0


instance Show RST where
    show (RST r s t) = show r ++ show s ++ show t
    show (RS r s)    = show r ++ show s
    show _           = ""

instance Read RST where
    readsPrec _ ss@(r:s:t:rest) = if Data.List.all isDigit (r:s:t:[]) then
                                    [(RST (digitToInt r) (digitToInt s) (digitToInt t), rest)] else
                                    [(NoRST, ss)]
    readsPrec _ ss@(r:s:rest)   = if Data.List.all isDigit (r:s:[]) then
                                    [(RS (digitToInt r) (digitToInt s), rest)] else
                                    [(NoRST, ss)]
    readsPrec _ ss              = [(NoRST, ss)]


$(deriveJSON defaultOptions ''Frequency)
$(deriveJSON defaultOptions ''RST)
$(deriveJSON defaultOptions ''QsoMode)
