module Ham.CAT.YaesuFT891 (yaesuFT891) where

import Ham.CAT.SerialCAT
import Ham.CAT.Parser
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
    serialGetFrequency = \s -> serialGet s "FA" frequencyFromAnswer
  , serialGetMode      = \s -> serialGet s "MD0" modeFromAnswer
  , serialGetPowerSSB = \s -> ft891GetMenu s "1601" intFromAnswer
  , serialSetPowerSSB = \s p -> ft891SetMenu s "1601" (printf "%.3d" p)
  , serialIdentify = identify_ft891
  }



identify_ft891 :: SerialPort -> IO Bool
identify_ft891 s = do
  mi <- serialGet s "ID" intFromAnswer
  case mi of
    Just 650 -> return True
    _ -> return False



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
