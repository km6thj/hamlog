module Ham.CAT.SerialCAT (SerialCAT(..)) where

import Ham.Internal.Data
import System.Hardware.Serialport


-- | Interface for radios that are communicating via serial interface.
data SerialCAT = SerialCAT {
    serialGetFrequency :: SerialPort -> IO (Maybe Frequency)
  , serialGetMode      :: SerialPort -> IO (Maybe QsoMode)
  , serialSetPowerSSB  :: SerialPort -> Int -> IO ()
  , serialGetPowerSSB  :: SerialPort -> IO (Maybe Int)
}
