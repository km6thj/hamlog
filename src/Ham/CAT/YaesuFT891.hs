module Ham.CAT.YaesuFT891 (yaesuFT891) where

import Ham.CAT.SerialCAT
import Ham.Internal.Data

import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport
import Data.Attoparsec.ByteString.Char8
import Text.Printf


----------------------------------------------------------------------------------
-- Yaesu FT891 stuff below this line.

yaesuFT891 :: SerialCAT
yaesuFT891 = SerialCAT {
  serialGetFrequency = \s -> ft891Get s "FA" frequencyFromAnswer
, serialGetMode      = \s -> ft891Get s "MD0" modeFromAnswer
, serialGetPowerSSB = \s -> ft891GetMenu s "1601" intFromAnswer
, serialSetPowerSSB = \s p -> ft891SetMenu s "1601" (printf "%.3d" p)
  }


ft891Get :: SerialPort
         -> String                     -- Command; e.g. FA for getting frequency of VFO A.
         -> (B.ByteString -> Parser a) -- Decoder for the resulting answer from the radio.
         -> IO (Maybe a)
ft891Get s cmd parser = do
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




-- | Menu command ("EX")
ft891Menu :: B.ByteString       -- Menu entry, P1 in the radio's CAT documentation
          -> Maybe B.ByteString -- OPtional value to set, P2 in the radio's CAT documentation
          -> B.ByteString       -- Resulting command to send to the radio.
ft891Menu p1 p2 = B.pack "EX" <> p1 <> (maybe mempty id p2)


ft891GetMenu :: SerialPort -> String -> (B.ByteString -> Parser a) -> IO (Maybe a)
ft891GetMenu s menuName valueParser =  do
    let menu = ft891Menu (B.pack menuName) Nothing
    send s $ (menu <> B.pack ";")
    threadDelay 100000
    a <- recv s 100
    return $
      let r = parse (valueParser menu) a
      in maybeResult r
      -- in case r of
      --   Done _ r' -> Just r'
      --   _         -> Nothing


ft891SetMenu :: SerialPort -> String -> String -> IO ()
ft891SetMenu s menuName value =  do
    let menu = ft891Menu (B.pack menuName) (Just $ B.pack value)
    send s $ (menu <> B.pack ";")
    -- putStrLn $ B.unpack menu
    threadDelay 100000
    _ <- recv s 100
    return ()
    -- putStrLn $ B.unpack a


parseFrequency :: B.ByteString -> Maybe Frequency
parseFrequency a = case r of
                     Done s r' -> Just $ MHz $ realToFrac $ r' * 1e-6 -- Assuming the number returned is in Hertz.
                     _         -> Nothing
  where r = parse (doubleFromAnswer (B.pack "FA")) a


frequencyFromAnswer :: B.ByteString
                    -> Parser Frequency
frequencyFromAnswer prefix = do
  string prefix
  v <- double
  char ';'
  return $ MHz $ realToFrac $ v * 1e-6 -- Assuming the number returned is in Hertz.


-- | Parse a double from an answer from the radio given the prefix.
doubleFromAnswer :: B.ByteString  -- Prefix to ignore from the answer.
                 -> Parser Double
doubleFromAnswer prefix = string prefix *> double <* char ';'


-- | Parse an integer from an answer from the radio given the prefix.
intFromAnswer :: B.ByteString -- Prefix to ignore from the answer.
              -> Parser Int
intFromAnswer prefix = string prefix *> decimal <* char ';'


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
             '6' -> RY
             '7' -> CW
             '8' -> DATA
             '9' -> RY
             'A' -> DATA
             'B' -> PH
             'C' -> DATA
             'D' -> PH
             _ -> PH
