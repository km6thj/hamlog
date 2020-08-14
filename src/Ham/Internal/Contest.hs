{-# LANGUAGE EmptyDataDecls, TypeFamilies #-}

module Ham.Internal.Contest where

import Ham.Log
import Ham.Data (band, Band)

import Data.Sequence as S
import Data.Char (toUpper)
import Data.Text as T
import qualified Data.Map as M
import Data.Proxy
import Data.List as L (groupBy, sortBy)
import Data.Foldable (toList)
import Prelude as P



class Contest a where
  type Points a :: *
  type Config a :: *
  computePoints :: Proxy a -> Config a -> HamLog (Points a)


data FieldDay


data FieldDayPoints = FieldDayPoints { fdpTotal :: Float
                                     , fdpPowerMult :: Float
                                     , fdpBonus :: Float
                                     , fdpQsos :: [(String, [(String, Int)])] } deriving Show


instance Contest FieldDay where
  type Points FieldDay = FieldDayPoints
  type Config FieldDay = ConfigFieldDay
  computePoints _ cfg = do
    s <- getQsoSeq
    let cw_qsos = S.filter (\a -> _qsoMode a == CW) s
        ph_qsos = S.filter (\a -> _qsoMode a == PH || _qsoMode a == FM) s
        data_qsos = S.filter (\a -> _qsoMode a == DATA || _qsoMode a == RY) s
        cw_points = 2 * fromIntegral (S.length cw_qsos)
        ph_points = 1 * fromIntegral (S.length ph_qsos)
        data_points = 2 * fromIntegral (S.length data_qsos)
        total = (cw_points + ph_points + data_points) * powerMult + bonus
        powerMult = fieldDayPowerMultiplier cfg
        bonus = fieldDayBonus cfg
    return $ FieldDayPoints total powerMult bonus [("CW", qsosByBand' cw_qsos),
                                                   ("PH", qsosByBand'  ph_qsos),
                                                   ("DATA", qsosByBand' data_qsos)]


qsosByBand' :: Seq Qso -> [(String, Int)]
qsosByBand' qsos = P.map f $ qsosByBand qsos
  where
    f as@(a:_) = ((show $ band $ _qsoFrequency a), P.length as)


-- | Group QSOs by band
qsosByBand :: Seq Qso -> [[Qso]]
qsosByBand qsos = groups
  where groups = L.groupBy (\a b -> band (_qsoFrequency a) == band (_qsoFrequency b)) sortedQsos
        sortedQsos = L.sortBy (\a b -> compare (_qsoFrequency a) (_qsoFrequency b)) $ toList qsos


qsosByBand'' :: Seq Qso -> [(Band, [Qso])]
qsosByBand'' qsos = P.map f $ qsosByBand qsos
  where f as@(a:_) = ((band $ _qsoFrequency a), as)


fieldDayBonus :: ConfigFieldDay -> Float
fieldDayBonus c = sum [c1,c2,c3,c4,c5,c6,fdOtherBonusPoints c]
  where c1 | fdMediaPublicity c = 100
           | otherwise = 0
        c2 | fdPublicLocation c = 100
           | otherwise = 0
        c3 | fdPublicInfoTable c = 100
           | otherwise = 0
        c4 = (fromIntegral $ fdFormalMessages c) * 10
        c5 | fdEmergencyPower c = 100
           | otherwise = 0
        c6 | fdAlternatePower c = 100
           | otherwise = 0


fieldDayPowerMultiplier :: ConfigFieldDay -> Float
fieldDayPowerMultiplier c =
  case fdPower c of
    FD5W -> 5
    FD150W -> 2
    FDHighPower -> 1



data FieldDayPower = FD5W | FD150W | FDHighPower

data ConfigFieldDay = ConfigFieldDay { fdEmergencyPower :: Bool  -- ^ Whether we are 100% on emergency power
                                     , fdPower :: FieldDayPower  -- ^ Whether we are <= 5 Watts for all contacts
                                     , fdMediaPublicity :: Bool  -- ^ Whether media publicity was done. Must provide proof.
                                     , fdPublicLocation :: Bool  -- ^ Whether we operate in a public location.
                                     , fdPublicInfoTable :: Bool -- ^ Whether there is an information table for people to stop by.
                                     , fdFormalMessages :: Int   -- ^ Number of formal messages relayed.
                                     , fdAlternatePower :: Bool  -- ^ Whether all batteries were charging from alternate power (no mains or gas).
                                     , fdOtherBonusPoints :: Float
                                     }


{--------------------------------------------------------------------------------}
{- NAQP CW -}


-- Get the state from a QSO exchange field, used for NAQP and possibly others.
-- The assumption is that the exchange is <something> and then the state as one word
-- as the last word, separated by whitespaces.
state qso = s
   where ex = _qsoExchange qso
         ws = T.words ex
         s | P.length ws >= 2 = T.toUpper $ P.last ws
           | otherwise = mempty

-- Counts the states in a list of QSOs. Used for NAQP to determine the state multiplier.
countStates qsos = M.size $ M.fromListWith (+) $ P.zip (P.map state qsos) (repeat 1)

naqpPoints :: QsoMode -> HamLog PointsNAQP
naqpPoints mode = do
    s <- getQsoSeq
    let mode_qsos = S.filter (\a -> _qsoMode a == mode)
        qsos_by_band = qsosByBand'' $ mode_qsos s
        multipliers_and_qso_count = P.map f qsos_by_band
          where f (bd, qsos) =
                  -- (band, multiplier, qso count)
                  (bd, fromIntegral $ countStates qsos, fromIntegral $ P.length qsos)

        -- Points per band
        mode_points = P.map (\(bd, mult, n) -> (bd, mult * n)) multipliers_and_qso_count
        mode_points_total = P.foldr ((+) . snd) 0 mode_points
    return $ PointsNAQP multipliers_and_qso_count mode_points_total


data PointsNAQP = PointsNAQP { pnaqpPointsPerBand :: [(Band, Float, Float)],
                               pnaqpTotalPoints :: Float } deriving Show


data NAQPCW


instance Contest NAQPCW where
  type Points NAQPCW = PointsNAQP
  type Config NAQPCW = ()
  computePoints _ cfg = naqpPoints CW


data NAQPSSB


instance Contest NAQPSSB where
  type Points NAQPSSB = PointsNAQP
  type Config NAQPSSB = ()
  computePoints _ cfg = naqpPoints PH
