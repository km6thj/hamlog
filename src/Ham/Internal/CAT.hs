{-# LANGUAGE DeriveAnyClass, FlexibleInstances, ScopedTypeVariables #-}

module Ham.Internal.CAT where

import Ham.CAT.SerialCAT
import Ham.CAT.YaesuFT891
import Ham.Internal.Data
import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport
import Control.Monad.Trans.RWS
import Control.Monad.IO.Class
import Control.Exception
import Data.Maybe (isJust)


data CATConfig = CATConfig { catPort :: String
                           , catSerialSettings :: SerialPortSettings }


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


catMode :: MonadIO m => CAT m (Maybe QsoMode)
catMode = CAT $ do
  ms <- gets statePort
  i <- gets stateInterface
  maybe (return Nothing) (\s -> liftIO $ serialGetMode i s) ms


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
