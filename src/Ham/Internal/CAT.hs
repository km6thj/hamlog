{-# LANGUAGE DeriveAnyClass, FlexibleInstances, ScopedTypeVariables #-}

module Ham.Internal.CAT where


import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport
import Ham.Internal.Data
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.RWS
import Control.Monad.IO.Class
import Control.Exception
import Data.Maybe (isJust)
import Data.Attoparsec.ByteString.Char8
import Text.Printf

data CATConfig = CATConfig { catPort :: String
                           , catSerialSettings :: SerialPortSettings
                           }


defaultConfig :: CATConfig
defaultConfig = CATConfig { catPort = "/dev/ttyUSB0"
                          , catSerialSettings = defaultSerialSettings { commSpeed = CS4800
                                                                      , bitsPerWord = fromIntegral 8
                                                                      , stopb = One
                                                                      , parity = NoParity
                                                                      , flowControl = NoFlowControl
                                                                      , timeout = 2 } }


data CATState = CATState { statePort :: Maybe SerialPort
                         , stateInterface :: SerialCAT }


defaultState :: CATState
defaultState = CATState { statePort = Nothing
                        , stateInterface = yaesuFT891 }


newtype CAT m a = CAT { unCAT :: RWST CATConfig () CATState m a }

instance Monad m => Functor (CAT m) where
  fmap f a = CAT $ fmap f (unCAT a)

instance Monad m => Applicative (CAT m) where
  pure = CAT . pure
  a <*> b = CAT $ unCAT a <*> unCAT b

instance Monad m => Monad (CAT m) where
  a >>= b = CAT $ unCAT a >>= (unCAT . b)


runCAT :: MonadIO m => CATConfig -> CATState -> CAT m a -> m a
runCAT config state act = fst <$> evalRWST (unCAT act') config state
  where
    act' = catInit *> act <* catDeinit


-- | FIXME: Add error handling. What if the radio can not be opened?
catInit :: MonadIO m => CAT m Bool
catInit = CAT $ do
  portName <- asks catPort
  serialSettings <- asks catSerialSettings

  ms <- liftIO $ catch
        (do { s <- openSerial portName serialSettings; return (Just s) })
        (\(e :: SomeException) -> {- putStrLn (show e) >> -} return Nothing)
  get >>= \a -> put (a { statePort = ms })
  return $ if isJust ms then True else False


catDeinit :: MonadIO m => CAT m ()
catDeinit = CAT $ do
  ms <- gets statePort
  maybe (return ()) (\s -> liftIO $ closeSerial s) ms
  get >>= \a -> put (a { statePort = Nothing })


catFrequency :: MonadIO m => CAT m (Maybe Frequency)
catFrequency = CAT $ do
  ms <- gets statePort
  i <- gets stateInterface
  maybe (return Nothing) (\s -> liftIO $ serialGetFrequency i s) ms


catPowerSSB :: MonadIO m => CAT m (Maybe Int)
catPowerSSB = CAT $ do
  ms <- gets statePort
  i <- gets stateInterface
  maybe (return Nothing) (\s -> liftIO $ serialGetPowerSSB i s) ms


catSetPowerSSB :: MonadIO m => Int -> CAT m ()
catSetPowerSSB power = CAT $ do
  ms <- gets statePort
  i <- gets stateInterface
  maybe (return ()) (\s -> liftIO $ serialSetPowerSSB i s power) ms


-- | Interface for radios that are communicating via serial interface.
data SerialCAT = SerialCAT {
    serialGetFrequency :: SerialPort -> IO (Maybe Frequency)
  , serialSetPowerSSB :: SerialPort -> Int -> IO ()
  , serialGetPowerSSB :: SerialPort -> IO (Maybe Int)
}


----------------------------------------------------------------------------------
-- Yaesu FT891 stuff below this line.

-- | Menu command ("EX")
ft891Menu :: B.ByteString       -- Menu entry, P1 in the radio's CAT documentation
          -> Maybe B.ByteString -- OPtional value to set, P2 in the radio's CAT documentation
          -> B.ByteString       -- Resulting command to send to the radio.
ft891Menu p1 p2 = B.pack "EX" <> p1 <> (maybe mempty id p2)


yaesuFT891 :: SerialCAT
yaesuFT891 = SerialCAT {
  serialGetFrequency = \s -> do
      send s $ B.pack "FA;"
      -- Wait for 100ms to give the transceiver some time to answer;
      -- I have not found a better way to do this at the moment with the serialport library.
      -- It seems to set all operations to non-blocking.
      -- Set the radio's CAT TOT to 10msec (minimum).
      threadDelay 100000
      a <- recv s 100
      -- putStrLn $ B.unpack a
      return $ parseFrequency a

, serialGetPowerSSB = \s -> ft891GetMenu s "1601" intFromAnswer
, serialSetPowerSSB = \s p -> ft891SetMenu s "1601" (printf "%.3d" p)
  }


ft891GetMenu :: SerialPort -> String -> (B.ByteString -> Parser a) -> IO (Maybe a)
ft891GetMenu s menuName valueParser =  do
    let menu = ft891Menu (B.pack menuName) Nothing
    send s $ (menu <> B.pack ";")
    threadDelay 100000
    a <- recv s 100
    return $
      let r = parse (valueParser menu) a
      in case r of
        Done _ r' -> Just r'
        _         -> Nothing


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



-- | Parse a double from an answer from the radio given the prefix.
doubleFromAnswer :: B.ByteString  -- Prefix to ignore from the answer.
                 -> Parser Double
doubleFromAnswer prefix = string prefix *> double <* char ';'


-- | Parse an integer from an answer from the radio given the prefix.
intFromAnswer :: B.ByteString -- Prefix to ignore from the answer.
              -> Parser Int
intFromAnswer prefix = string prefix *> decimal <* char ';'
