{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables #-}

module Ham.Internal.Log.Config (LogConfig
                               ,ToLogConfig
                               ,defaultConfig
                               ,configFromFile
                               ,configToFile
--                               ,module V1
--                               ,module V2
                               ,module Ham.Internal.Log.V3)

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
import qualified Ham.Internal.Log.V2 as V2
import qualified Ham.Internal.Log.V3 as V3
import Ham.Internal.Log.V3
import Ham.Qso
import qualified Ham.CAT as CAT


type LogConfig = V3.LogConfigV3


class ToLogConfig a b where
  toLogConfig :: a -> b


instance ToLogConfig V1.LogConfigV1 V2.LogConfigV2 where
  toLogConfig (V1.LogConfigV1 logfile qsodefaults) = V2.LogConfigV2 { V2._configLogFile = logfile,
                                                                      V2._configQsoDefaults = qsodefaults,
                                                                      V2._configCat = CAT.defaultConfig }

instance ToLogConfig V1.LogConfigV1 V3.LogConfigV3 where
  toLogConfig a = toLogConfig b
    where b = toLogConfig a :: V2.LogConfigV2


instance ToLogConfig V2.LogConfigV2 V2.LogConfigV2 where
  toLogConfig = id


instance ToLogConfig V2.LogConfigV2 V3.LogConfigV3 where
  toLogConfig (V2.LogConfigV2 logfile qsodefaults catconf) = V3.LogConfigV3 { V3._configLogFile = logfile,
                                                                              V3._configQsoDefaults = qsodefaults,
                                                                              V3._configUseCat = False,
                                                                              V3._configCat = CAT.defaultConfig }

instance ToLogConfig V3.LogConfigV3 V3.LogConfigV3 where
  toLogConfig = id


defaultConfig :: LogConfig
defaultConfig = V3.LogConfigV3 { V3._configLogFile = "hamlog.json" -- This is the name of the log database file.
                               , V3._configQsoDefaults = emptyQsoDefaults
                               , V3._configUseCat = False
                               , V3._configCat = CAT.defaultConfig
                               }


-- | Read the log configuration from a text file.
configFromFile :: FilePath -> IO (Maybe LogConfig)
configFromFile fp = do
  let p1 :: Proxy V1.LogConfigV1
      p1 = Proxy
      p2 :: Proxy V2.LogConfigV2
      p2 = Proxy
      p3 = Proxy :: Proxy V3.LogConfigV3
  let a = sequence [configFromFile' fp p3, configFromFile' fp p2, configFromFile' fp p1]
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
