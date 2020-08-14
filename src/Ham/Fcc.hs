{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Ham.Fcc
  (fccLookup',
   fccLookup,
   fccLookupName,
   fccLicenseFromResult,
   FccResult,
   FccLicense(..),
   FccLicenses,
   licenses
  ) where

import Network.HTTP
import Data.Aeson
import Data.Aeson.TH
import Data.Array
import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as H

import Ham.Internal.Fcc

type Address = String
type Name = String

-- $(deriveJSON (defaultOptions { fieldLabelModifier = resultLabelMod, constructorTagModifier = resultConstructorMod }) ''FccResult)
$(deriveJSON defaultOptions ''FccLicense)

instance FromJSON FccResult where
  parseJSON (Object v) = FccResult
    <$> v .: "status"
    <*> v .: "Licenses"

instance FromJSON FccLicenses where
  parseJSON (Object v) = FccLicenses
    <$> v .: "lastUpdate"
    <*> v .: "page"
    <*> v .: "totalRows"
    <*> v .: "License"
    <*> v .: "rowPerPage"


-- | Lookup an entry for a callsign in the FCC online database.
-- If an entry can be found, it is returned.
fccLookup' :: Text  -- ^ The callsign to search for.
           -> IO (Maybe FccResult)
fccLookup' callsign = do
  let rq_string = "http://data.fcc.gov/api/license-view/basicSearch/getLicenses?searchValue=" ++ T.unpack callsign ++ "&format=json"
  r <- simpleHTTP $ getRequest rq_string
  case r of
    Left err -> return Nothing
    Right response -> do
      let o = decode (B.pack $ map (fromIntegral . fromEnum) $ rspBody response) :: Maybe Object
      let eo2 = eitherDecode (B.pack $ map (fromIntegral . fromEnum) $ rspBody response) :: Either String FccResult
      case eo2 of
        Right a -> return $ Just a
        Left s -> putStrLn s >> return Nothing


-- | Look up the first active license given a callsign.
fccLookup :: Text -> IO (Maybe FccLicense)
fccLookup cs = do { a <- fccLookup' cs; return $ a >>= fccLicenseFromResult }


-- | Look up the operator's name from the first active license given a callsign.
fccLookupName :: Text -> IO Text
fccLookupName cs = do
  a <- fccLookup' cs
  maybe (return "") (return . T.pack . licName) (a >>= fccLicenseFromResult)



fccLicenseFromResult :: FccResult -> Maybe FccLicense
fccLicenseFromResult r = listToMaybe ls
  where ls = license $ licenses r
        active_ls = filter (( == "active") . map toLower . statusDesc) ls
