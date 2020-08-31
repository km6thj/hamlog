module Ham.CAT.SerialCAT (SerialCAT(..)
                         ,serialGet
                         ,serialSend) where

import Ham.Internal.Data
import System.Hardware.Serialport
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Control.Concurrent (threadDelay)


-- | Interface for radios that are communicating via serial interface.
data SerialCAT = SerialCAT {
    serialGetFrequency :: SerialPort -> IO (Maybe Frequency)
  , serialGetMode      :: SerialPort -> IO (Maybe QsoMode)
  , serialSetPowerSSB  :: SerialPort -> Int -> IO ()
  , serialGetPowerSSB  :: SerialPort -> IO (Maybe Int)
  , serialIdentify     :: SerialPort -> IO Bool
}


-- | Get a value from a radio connected to a serial port.
-- This uses a format that seems to be used by more than one manufacturer, such as Yaesu and Elecraft.
-- The command format is "XY;" for getting something, where XY is a string. XY is given as second argument to this function.
serialGet :: SerialPort
         -> String                     -- ^ Command; e.g. FA for getting frequency of VFO A.
         -> (B.ByteString -> Parser a) -- ^ Decoder for the resulting answer from the radio.
         -> IO (Maybe a)
serialGet s cmd parser = do
  let cmd' = B.pack cmd <> B.pack ";"
  send s cmd'
  -- Wait for 100ms to give the transceiver some time to answer;
  -- I have not found a better way to do this at the moment with the serialport library.
  -- It seems to set all operations to non-blocking.
  -- Set the radio's CAT TOT to 10msec (minimum).
  threadDelay 100000
  a <- recv s 100
  -- putStrLn $ B.unpack a

  return $
      let r = parse (parser $ B.pack cmd) a
      in maybeResult r


-- | Send a string to the radio.
serialSend :: SerialPort -> String -> IO ()
serialSend s value =  do
    send s $ B.pack value
    threadDelay 100000
    _ <- recv s 100
    return ()
    -- putStrLn $ B.unpack a
