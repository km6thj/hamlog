{-# LANGUAGE DeriveAnyClass, FlexibleInstances, ScopedTypeVariables, DeriveGeneric, StandaloneDeriving, OverloadedStrings #-}

{-| Computer Aided Transceiver interface.
This provides a monad that allows to talk to serially connected transceivers.
To support a transceiver, there must be a `SerialCAT' provided for it.
-}
module Ham.Internal.CAT where

import Control.Monad (when)
import Control.Monad.Trans.RWS
import Control.Monad.IO.Class
import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics
import Ham.CAT.SerialCAT
import Ham.CAT.Radios
import Ham.Internal.Data
import System.Hardware.Serialport



-- | Configuration for the CAT monad.
data CATConfig = CATConfig { catPort :: String  -- ^ Serial port the radio is connected to. In Linux e.g. "/dev/ttyUSB0".
                           , catRadio :: Radio  -- ^ Radio identifier.
                           , catSerialSettings :: SerialPortSettings  -- ^ Serial settings for the connection. Note the radio must have the same settings.
                           } deriving (Generic, Show)

-- Automatically derive Show instance. That's not inclded in the serialport library.
deriving instance Show SerialPortSettings
deriving instance Generic SerialPortSettings
deriving instance Generic CommSpeed
deriving instance Generic FlowControl
deriving instance Generic StopBits
deriving instance Generic Parity

instance FromJSON SerialPortSettings
instance ToJSON SerialPortSettings
instance FromJSON CommSpeed
instance ToJSON CommSpeed
instance FromJSON FlowControl
instance ToJSON FlowControl

instance FromJSON StopBits
instance ToJSON StopBits
instance FromJSON Parity
instance ToJSON Parity

instance FromJSON CATConfig
instance ToJSON CATConfig

instance FromJSON Radio
instance ToJSON Radio


-- | Default configuration. Note this assumes port and radio, so you will want to adjust it.
defaultConfig :: CATConfig
defaultConfig = CATConfig { catPort = "/dev/ttyUSB0"
                          , catRadio = YaesuFT891
                          , catSerialSettings = defaultSerialSettings { commSpeed = CS4800
                                                                      , bitsPerWord = fromIntegral 8
                                                                      , stopb = One
                                                                      , parity = NoParity
                                                                      , flowControl = NoFlowControl
                                                                      , timeout = 2 } }

-- | State type for the CAT monad.
data CATState = CATState { statePort :: Maybe SerialPort  -- ^ Serial port, if one is open.
                         , stateInterface :: SerialCAT    -- ^ SerialCAT implementation for the radio.
                         }


-- | Default state. Note that this sets the SerialCAT to a default implementation. You will want to change that.
defaultState :: CATState
defaultState = CATState { statePort = Nothing
                        , stateInterface = yaesuFT891 }


-- | The computer aided transceiver monad.
newtype CAT m a = CAT { unCAT :: RWST CATConfig [Text] CATState m a }

instance Monad m => Functor (CAT m) where
  fmap f a = CAT $ fmap f (unCAT a)

instance Monad m => Applicative (CAT m) where
  pure = CAT . pure
  a <*> b = CAT $ unCAT a <*> unCAT b

instance Monad m => Monad (CAT m) where
  a >>= b = CAT $ unCAT a >>= (unCAT . b)


-- | Run an action in the CAT monad.
runCAT :: MonadIO m => CATConfig
       -> CATState -- ^ State to start with.
       -> CAT m a
       -> m (a, [Text])
runCAT config state act = evalRWST (unCAT act') config state
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
  let ok = if isJust ms then True else False
  if ok
    then do
      ident <- serialIdentify <$> gets stateInterface
      a <- maybe (return False) (\s -> liftIO $ ident s) ms
      tell $ ["Identifying configured radio: " <> T.pack (show a)]
      return a
    else return False


-- | Close everything down.
catDeinit :: MonadIO m => CAT m ()
catDeinit = CAT $ do
  ms <- gets statePort
  maybe (return ()) (\s -> liftIO $ closeSerial s) ms
  get >>= \a -> put (a { statePort = Nothing })


-- | Get the current frequency from the transceiver.
catFrequency :: MonadIO m => CAT m (Maybe Frequency)
catFrequency = CAT $ do
  ms <- gets statePort
  i <- gets stateInterface
  maybe
    (tell ["Could not get frequency from radio"] >> return Nothing)
    (\s -> do
        f <- liftIO $ serialGetFrequency i s
        tell ["Got frequency " <> T.pack (show f)]
        return f
    )
    ms


-- | Get the current mode from the transceiver.
catMode :: MonadIO m => CAT m (Maybe QsoMode)
catMode = CAT $ do
  ms <- gets statePort
  i <- gets stateInterface
  maybe
    (tell ["Could not get mode from radio."] >> return Nothing)
    (\s -> do
        a <- liftIO $ serialGetMode i s
        tell ["Got mode " <> T.pack (show a)]
        return a
    )
    ms


-- | Get the SSB power setting in watts.
catPowerSSB :: MonadIO m => CAT m (Maybe Int)
catPowerSSB = CAT $ do
  ms <- gets statePort
  i <- gets stateInterface
  maybe (return Nothing) (\s -> liftIO $ serialGetPowerSSB i s) ms


-- | Set the SSB power in watts.
catSetPowerSSB :: MonadIO m => Int -> CAT m ()
catSetPowerSSB power = CAT $ do
  ms <- gets statePort
  i <- gets stateInterface
  maybe (return ()) (\s -> liftIO $ serialSetPowerSSB i s power) ms
