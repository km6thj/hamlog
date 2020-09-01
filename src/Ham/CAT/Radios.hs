{-# LANGUAGE DeriveGeneric #-}

{-| Definitions for supported radios. -}
module Ham.CAT.Radios (Radio(..)
                      ,serialInterface
                      ,module Ham.CAT.YaesuFT891
                      ,module Ham.CAT.ElecraftKX2) where


import Data.Map
import Ham.CAT.YaesuFT891
import Ham.CAT.ElecraftKX2
import Ham.CAT.SerialCAT
import GHC.Generics


-- | Identifier for each of the supported radios.
-- The names are more or less self-explanatory.
data Radio = YaesuFT891
           | ElecraftK3 -- ^ This includes K3, KX2, KX3.
           deriving (Show, Generic, Ord, Eq)


-- | Map from radio identifiers to serial interfaces.
-- This is used e.g. when reading a radio identifier from a configuration file,
-- to instantiate the correct `SerialCAT'.
serialInterface :: Map Radio SerialCAT
serialInterface = fromList [(YaesuFT891, yaesuFT891)
                           ,(ElecraftK3, elecraftKX2)]
