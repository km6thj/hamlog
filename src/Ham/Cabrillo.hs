{-# LANGUAGE OverloadedStrings #-}

module Ham.Cabrillo
 where

import Data.Maybe (isJust, fromJust)
import Data.Text as T
import Data.Text (unpack)
import Data.List as L
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Format
import Ham.Data

data VersionNumber = VersionNumber Int Int
instance Show VersionNumber where
    showsPrec _ (VersionNumber a b) s = L.concat [show a, ".", show b] ++ s

data CategoryAssisted = Assisted | NonAssisted
instance Show CategoryAssisted where
    show Assisted = "ASSISTED"
    show NonAssisted = "NON-ASSISTED"

data CategoryMode = CMCW | CMDIGI | CMFM | CMRTTY | CMSSB | CMMIXED
instance Show CategoryMode where
    show CMCW = "CW"
    show CMDIGI = "DIGI"
    show CMFM = "FM"
    show CMRTTY = "RTTY"
    show CMSSB = "SSB"
    show CMMIXED = "MIXED"

data CategoryOperator = SINGLE_OP | MULTI_OP | CHECKLOG
instance Show CategoryOperator where
    show SINGLE_OP = "SINGLE-OP"
    show MULTI_OP = "MULTI-OP"
    show CHECKLOG = "CHECKLOG"

data CategoryPower = HIGH | LOW | QRP deriving (Show, Read)

data CategoryStation = FIXED | MOBILE | PORTABLE | ROVER
                     | ROVER_LIMITED | ROVER_UNLIMITED | EXPEDITION | HQ | SCHOOL
instance Show CategoryStation where
    show FIXED = "FIXED"
    show MOBILE = "MOBILE"
    show PORTABLE = "PORTABLE"
    show ROVER = "ROVER"
    show ROVER_LIMITED = "ROVER-LIMITED"
    show ROVER_UNLIMITED = "ROVER-UNLIMITED"
    show EXPEDITION = "EXPEDITION"
    show HQ = "HQ"
    show SCHOOL = "SCHOOL"

data CategoryTime = HOURS_6 | HOURS_12 | HOURS_24
instance Show CategoryTime where
    show HOURS_6 = "6-HOURS"
    show HOURS_12 = "12-HOURS"
    show HOURS_24 = "24-HOURS"

data CategoryTransmitter = ONE | TWO | LIMITED | UNLIMITED | SWL deriving (Show, Read)
data CategoryOverlay = CLASSIC | ROOKIE | TB_WIRES | NOVICE_TECH | OVER_50
instance Show CategoryOverlay where
    show CLASSIC = "CLASSIC"
    show ROOKIE = "ROOKIE"
    show TB_WIRES = "TB-WIRES"
    show NOVICE_TECH = "NOVICE-TECH"
    show OVER_50 = "OVER-50"


data YesNo = YES | NO deriving (Show, Read)

data Cabrillo = Cabrillo
            {
                version :: VersionNumber
            , callsign :: Maybe Text
            , contest :: Maybe Text
            , category_assisted :: Maybe CategoryAssisted
            , category_band :: Maybe Text
            , category_mode :: Maybe CategoryMode
            , category_operator :: Maybe CategoryOperator
            , category_power :: Maybe CategoryPower
            , category_station :: Maybe CategoryStation
            , category_time :: Maybe CategoryTime
            , category_transmitter :: Maybe CategoryTransmitter
            , category_overlay :: Maybe CategoryOverlay
            , certificate :: Maybe YesNo
            , claimed_score :: Maybe Integer
            , club :: Maybe Text
            , created_by :: Maybe Text
            , email :: Maybe Text
            , grid_locator :: Maybe Text
            , location :: Maybe Text -- Note: This is restricted depending on the contest. See http://wwrof.org/cabrillo/cabrillo-specification-v3/
            , name :: Maybe Text
            , address_city :: Maybe Text
            , address_state_province :: Maybe Text
            , address_postalcode :: Maybe Text
            , address_country :: Maybe Text
            , operators :: Maybe [Text]
            }

defaultCabrillo = Cabrillo (VersionNumber 3 0)
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing



showtext d f a = fmap (\b -> L.concat [a, unpack b]) $ f d
showother d f a = fmap (\b -> L.concat [a, show b]) $ f d
showlist :: d -> (d -> Maybe [Text]) -> String -> Maybe String
showlist d f a = fmap (\b -> a ++ L.unwords (L.map unpack b)) $ f d

instance Show Cabrillo where
    show c = L.unlines $ L.map fromJust $ L.filter isJust [
         showtext  c callsign "CALLSIGN: "
        ,showtext  c contest "CONTEST: "
        ,showother c category_assisted "CATEGORY-ASSISTED: "
        ,showtext  c category_band "CATEGORY-BAND: "
        ,showother c category_mode "CATEGORY-MODE: "
        ,showother c category_operator "CATEGORY-OPERATOR: "
        ,showother c category_power "CATEGORY-POWER: "
        ,showother c category_station "CATEGORY-STATION: "
        ,showother c category_time "CATEGORY-TIME: "
        ,showother c category_transmitter "CATEGORY-TRANSMITTER: "
        ,showother c category_overlay "CATEGORY-OVERLAY: "
        ,showother c certificate "CERTIFICATE: "
        ,showother c claimed_score "CLAIMED-SCORE: "
        ,showtext  c club "CLUB: "
        ,showtext  c created_by "CREATED-BY: "
        ,showtext  c email "EMAIL: "
        ,showtext  c grid_locator "GRID-LOCATOR: "
        ,showtext  c location "LOCATION: "
        ,showtext  c name "NAME: "
        ,showtext  c address_city "ADDRESS-CITY: "
        ,showtext  c address_state_province "ADDRESS-STATE-PROVINCE: "
        ,showtext  c address_postalcode "ADDRESS-POSTALCODE: "
        ,showtext  c address_country "ADDRESS-COUNTRY: "
        ,showlist  c operators "OPERATORS: "]





data CabrilloLine = CLSoapbox Text | CLQso CabrilloQso
                  | CLX Text Text | CLXQso CabrilloQso

instance Show CabrilloLine where
    show (CLSoapbox t) = "SOAPBOX: " ++ unpack t
    show (CLQso a) = "QSO: " ++ show a
    show (CLX a b) = "X-" ++ unpack a ++ ": " ++ unpack b
    show (CLXQso a) = "X-QSO: " ++ show a

-- Note: OffTime is missing.
data CabrilloQso = CabrilloQso
                        Frequency Mode UTCTime
                        CabrilloInfoSent CabrilloInfoReceived
                        (Maybe TransmitterNumber)

data CabrilloInfo = CabrilloInfo Callsign Exchange

instance Show CabrilloInfo where
    show (CabrilloInfo cs exchg) = L.unwords [cs, exchg]

type Exchange = String
type Callsign = String
type Location = String
type CabrilloInfoSent = CabrilloInfo
type CabrilloInfoReceived = CabrilloInfo
type TransmitterNumber = Int
data Mode = CW | PH | FM | RY | DG deriving (Show, Read)

instance Show CabrilloQso where
    show (CabrilloQso (MHz f) m t s r mn) = L.unwords $
        [
         show $ round (f * 1000)
        ,show m
        ,date
        ,tm
        ,show s
        ,show r
        ,maybe [] show mn
        ]
        where date = formatTime defaultTimeLocale "%F" t
              tm   = formatTime defaultTimeLocale "%H%M" t


data CabrilloLog = CabrilloLog Cabrillo [CabrilloLine]

instance Show CabrilloLog where
    show (CabrilloLog c ls) =
        L.unlines $ [
            "START-OF-LOG: 3.0"
            ,show c
            ,L.unlines (L.map show ls)
            ,"END-OF-LOG:"]


class ToCabrillo a where
    toCabrillo :: a -> [CabrilloLine]
