{-# LANGUAGE DeriveGeneric #-}

module Ham.CAT.Radios (Radio(..)
                      ,serialInterface
                      ,module Ham.CAT.YaesuFT891
                      ,module Ham.CAT.ElecraftKX2) where


import Data.Map
import Ham.CAT.YaesuFT891
import Ham.CAT.ElecraftKX2
import Ham.CAT.SerialCAT
import GHC.Generics


data Radio = YaesuFT891
           | ElecraftK3 deriving (Show, Generic, Ord, Eq)


serialInterface :: Map Radio SerialCAT
serialInterface = fromList [(YaesuFT891, yaesuFT891)
                           ,(ElecraftK3, elecraftKX2)]
