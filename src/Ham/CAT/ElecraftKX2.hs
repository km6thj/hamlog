module Ham.CAT.ElecraftKX2 (elecraftKX2) where

import Ham.CAT.SerialCAT
import Ham.CAT.Parser
import Ham.Internal.Data

import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport
import Data.Attoparsec.ByteString.Char8
import Text.Printf


----------------------------------------------------------------------------------
-- Yaesu FT891 stuff below this line.

elecraftKX2 :: SerialCAT
elecraftKX2 = SerialCAT {
    serialGetFrequency = \s -> serialGet s "FA" frequencyFromAnswer
  , serialGetMode      = \s -> serialGet s "MD" modeFromAnswer
  , serialGetPowerSSB = \s -> serialGet s "PC" intFromAnswer
  , serialSetPowerSSB = \s p -> serialSend s (printf "PC%.3d;" p)
  , serialIdentify = identify_kx2
  }


identify_kx2 :: SerialPort -> IO Bool
identify_kx2 s = do
  mi <- serialGet s "K3" intFromAnswer
  case mi of
    Just _ -> return True
    _ -> return False


modeFromAnswer :: B.ByteString
               -> Parser QsoMode
modeFromAnswer prefix = do
  string prefix
  c <- anyChar
  char ';'
  return $ case c of
             '1' -> PH
             '2' -> PH
             '3' -> CW
             '4' -> FM
             '5' -> PH
             '6' -> DATA
             '7' -> CW
             '9' -> DATA
             _ -> PH
