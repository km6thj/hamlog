{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables #-}

module Ham.Internal.Log.Config (LogConfig
                               ,ToLogConfig
                               ,defaultConfig
                               ,configFromFile
                               ,configToFile
                               ,module V1
                               ,module V2)

  where

import Control.Exception
import Control.Monad (liftM)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Maybe (listToMaybe, catMaybes)
import Data.Proxy
import qualified Data.ByteString.Lazy as B
import Ham.Internal.Qso
import qualified Ham.Internal.Log.V1 as V1
import Ham.Internal.Log.V2 as V2
import Ham.Qso
import qualified Ham.CAT as CAT


type LogConfig = LogConfigV2


class ToLogConfig a b where
  toLogConfig :: a -> b


instance ToLogConfig V1.LogConfigV1 LogConfigV2 where
  toLogConfig (V1.LogConfigV1 logfile qsodefaults) = LogConfigV2 { _configLogFile = logfile,
                                                                   _configQsoDefaults = qsodefaults,
                                                                   _configCat = CAT.defaultConfig }

instance ToLogConfig LogConfigV2 LogConfigV2 where
  toLogConfig = id


defaultConfig :: LogConfig
defaultConfig = LogConfigV2 { _configLogFile = "hamlog.json" -- This is the name of the log database file.
                            , _configQsoDefaults = emptyQsoDefaults
                            , _configCat = CAT.defaultConfig
                            }


-- | Read the log configuration from a text file.
configFromFile :: FilePath -> IO (Maybe LogConfig)
configFromFile fp = do
  let p1 :: Proxy V1.LogConfigV1
      p1 = Proxy
      p2 :: Proxy LogConfigV2
      p2 = Proxy
  let a = sequence [configFromFile' fp p2, configFromFile' fp p1]
  result <- (liftM $ listToMaybe . catMaybes) a
  return result

--configFromFile :: FilePath -> IO (Maybe LogConfig)
--configFromFile fp = (decode <$> B.readFile fp)
--      `catch` \(SomeException e) -> return Nothing


configFromFile' :: forall a. (ToLogConfig a LogConfig, FromJSON a) => FilePath -> Proxy a -> IO (Maybe LogConfig)
configFromFile' fp _ =
  (do
    ma <- (decode <$> B.readFile fp) :: IO (Maybe a)
    return $ fmap toLogConfig ma)
  `catch` \(SomeException e) -> return Nothing


-- | Write the log configuration to a text file.
configToFile :: LogConfig -> FilePath -> IO ()
configToFile c fp =
  B.writeFile fp $ encodePretty c
