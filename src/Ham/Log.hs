{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

{-| Ham radio logging and other operations.
This module provides a monad that abstracts some of the work
necessary for logging ham radio contacts, as well as doing related
tasks.
-}
module Ham.Log
    (module Ham.Qso,
     -- * Reading and writing log and configuration.
     logToFile,
     logFromFile,
     configToFile,
     configFromFile,
     -- * The HamLog monad.
     HamLog,
     LogState(..),
     emptyLogState,
     -- ** Configuration
     LogConfigV3(..),
     LogConfig(..),
     configLogFile,
     configQsoDefaults,
     defaultConfig,
     -- ** Running the monad
     evalHamLog,
     execHamLog,
     runHamLog,
     -- ** Reading and writing logs
     readLog,
     writeLog,
     -- ** Manipulating QSOs.
     addQso,
     newQsoNow,
     fillFromCAT,
     updateQso,
     deleteQso,
     sortLog,
     -- ** Helpers
     currentUtcTime,
     -- ** Queries
     filterCallsign,
     isDuplicate,
     findQsos,
     findDuplicateQsos,
     Duplicate(..),
     getQsoList,
     getQsoSeq,
     asks,
     -- ** Interacting with a radio via CAT
     cat,
     -- ** Getting information from the FCC database.
     lookupFcc,
     lookupFccName,
     -- * The Log
     Log(..),
     addQsoToLog,
     sortLog_,
     emptyLog,
     -- * Exporting
     makeCabrillo
) where


import Data.Foldable
import qualified Data.Sequence as S
import Data.Sequence (Seq)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

import Data.Text as T
import Data.Sequence
import qualified Data.ByteString.Lazy as B
import Data.Aeson.Encode.Pretty
import Data.Aeson
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as M
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Trans.RWS.Strict
import Control.Monad.IO.Class
import Control.Exception

import Ham.Internal.Log
import Ham.Internal.Log.Config
import Ham.Fcc
import Ham.Cabrillo
import Ham.Qso
import Ham.Data
import qualified Ham.CAT as CAT
import Ham.CAT.Radios

import Lens.Micro.TH


-- | An empty log.
emptyLog :: Log
emptyLog = Log mempty


-- | Add a QSO to the beginning of the log list.
addQsoToLog :: Log -> Qso -> Log
addQsoToLog l q = l { _logQsos = m }
  where m = q <| _logQsos l


sortLog_ :: Ord a => Log -> (Qso -> a) -> Log
sortLog_ l g = l { _logQsos = sortBy f (_logQsos l) }
  where f a b = compare (g a) (g b)


-- | Write a log to a file in JSON format.
logToFile :: Log -> FilePath -> IO ()
logToFile l fp = B.writeFile fp $ encodePretty l


-- | Read a log from a file in JSON format.
logFromFile :: FilePath -> IO (Maybe Log)
logFromFile fp =
  (decode <$> B.readFile fp)
    `catch` \(SomeException e) -> return Nothing



--------------------------------------------------------------------------------
-- Log monad.

{-| Log monad.
You would normally run the monad for example with 'evalHamLog',
and use the monadic actions such as 'readLog', 'newQsoNow', 'writeLog', etc.
to modify the log and QSOs. -}
type HamLog = RWST LogConfig [Text] LogState IO

data LogState = LogState { _stateLog :: Log
                         , _stateUseCat :: Bool   -- ^ Mirroring the configuration in _configCat in the LogConfig to be able to turn it off after a failed action.
                         , _stateSerialCAT :: Maybe CAT.SerialCAT }

makeLenses ''LogState


emptyLogState :: LogState
emptyLogState = LogState emptyLog True Nothing


initState :: HamLog ()
initState = do
  catconf <- asks _configCat
  -- Set the serial radio interface if there is one.
  let mserial = M.lookup (CAT.catRadio catconf) serialInterface
  s <- get
  c <- asks _configUseCat
  put s { _stateSerialCAT = mserial
        , _stateUseCat = c }
  -- maybe (return ()) (\serial -> do { s <- get; put s { _stateSerialCAT = Just serial } }) mserial




-- | Run a 'HamLog' action and return the result and potential logging text.
evalHamLog :: LogConfig -> LogState -> HamLog a -> IO (a, [Text])
evalHamLog cfg s act = evalRWST (initState >> act) cfg s


-- | Run a 'HamLog' action and return the final state and potential logging text.
execHamLog :: LogConfig -> LogState -> HamLog a -> IO (LogState, [Text])
execHamLog cfg s act = execRWST (initState >> act) cfg s


-- | Run a 'HamLog' action and return the result, final state, and potential logging text.
runHamLog :: LogConfig -> LogState -> HamLog a -> IO (a, LogState, [Text])
runHamLog cfg s act = runRWST (initState >> act) cfg s


-- | Read the log database from the file set in '_configLogFile' entry of the 'LogConfig'.
-- The log is then set to the '_stateLog' entry of the current 'LogState'.
readLog :: HamLog ()
readLog = do
  f <- asks _configLogFile
  ml <- liftIO $ logFromFile f `catch` \(SomeException a) -> return Nothing
  maybe (return ()) (\l -> do { s <- get; put $ s { _stateLog = l } }) ml


-- | Write the log database to the file set in '_configLogFile' entry of the 'LogConfig'.
writeLog :: HamLog ()
writeLog = do
  f <- asks _configLogFile
  l <- gets _stateLog

  liftIO $ logToFile l f


-- | Add the given QSO at the front of the current log.
addQso :: Qso -> HamLog ()
addQso q = do
  l <- gets _stateLog
  let l' = addQsoToLog l $ sanitizeQso q
  modify $ \s -> s { _stateLog = l' }


-- | Get the current UTC time up to seconds.
currentUtcTime :: HamLog UTCTime
currentUtcTime = liftIO $ do
    t <- getCurrentTime
    let tod = utctDayTime t
        -- Remove anything that is more accurate than one second. We assume that is enough for
        -- our purpose.
        tod' = (picosecondsToDiffTime . (\a -> a - a `rem` (10^12)) . diffTimeToPicoseconds) tod
    return $ t { utctDayTime = tod' }


-- | Create a new QSO with the current UTC time as reported by the operating system,
-- and set the default values from the '_configQsoDefaults' values.
-- uses `fillFromCAT` to fill some values from a connected radio, if any.
newQsoNow :: HamLog Qso
newQsoNow = do
  now <- currentUtcTime
  defaults <- asks _configQsoDefaults
  let q = (qsoWithDefaults defaults)
            { _qsoTimeStart = now
            , _qsoTimeEnd = (fromIntegral 60) `addUTCTime` now
            }
  q' <- fillFromCAT q
  addQso q'
  return q'


-- | Try to get a few values from a CAT connected radio.
-- If the function is turned off (_stateUseCat is False), then the input is going to be
-- returned unchanged. The same is true if there is an error while retrieving
-- data from the radio.
-- Note that when the functionality is turned on and there is an error, it may take a few seconds
-- for the function to return. A message will be set in the Writer layer of HamLog,
-- so that you can get the messages back with `evalHamLog`, `execHamLog`, or `runHamLog`.
fillFromCAT :: Qso -> HamLog Qso
fillFromCAT qso = do
  result <- cat $ do
    f <- CAT.catFrequency
    m <- CAT.catMode
    return (f,m)
  case result of
    Left (CATError t s) -> tell [s] >> return qso
    Right (mf, mm) -> return $ qso { _qsoFrequency = maybe (_qsoFrequency qso) id mf,
                                     _qsoMode = maybe (_qsoMode qso) id mm }



-- | Update the QSO at the given position in the log with the given QSO.
updateQso :: Int -> Qso -> HamLog ()
updateQso i q = do
  l <- gets _stateLog
  let q' = update i (sanitizeQso q) $ _logQsos l
  modify $ \s -> s { _stateLog = l { _logQsos = q' } }
  return ()


-- | Delete the QSO at the given position in the log.
deleteQso :: Int -> HamLog ()
deleteQso i = do
  l <- gets _stateLog
  let q' = deleteAt i $ _logQsos l
  modify $ \s -> s { _stateLog = l { _logQsos = q' } }


-- | Sort the log according to the given key.
sortLog :: Ord a => (Qso -> a) -> HamLog ()
sortLog g = do
  l <- gets _stateLog
  let l' = sortLog_ l g
  modify $ \s -> s { _stateLog = l' }


-- | Get a list of QSOs. Use qsoSeq for better performance.
getQsoList :: HamLog [Qso]
getQsoList = do
  l <- _logQsos <$> gets _stateLog
  return $ Data.Foldable.foldr (:) [] l


-- | Get the sequence of stored QSOs.
getQsoSeq :: HamLog (Seq Qso)
getQsoSeq = _logQsos <$> gets _stateLog


-- | Return a sequence of all QSOs with a given callsign.
filterCallsign :: Text             -- ^ The Callsign to find.
               -> HamLog (Seq Qso) -- ^ Returns a sequence of Qsos.
filterCallsign callsign = do
  l <- getQsoSeq
  let f q = let cs = T.toUpper $ _qsoCallsign q
                cs' = T.strip $ T.toUpper callsign
            in cs == cs'
  return (S.filter f l)


-- | Checks if there is already at least one entry with the given callsign.
-- DEPRECATED. Use findDuplicateQsos.
isDuplicate :: Text        -- ^ The Callsign to find.
            -> HamLog Bool
isDuplicate callsign = do
  s <- filterCallsign callsign
  case s of
    Empty -> return False
    _ -> return True



-- | Find qsos containing the given information.
findQsos :: Text -- ^ The callsign to find.
         -> Maybe Band -- ^ Band to check for
         -> Maybe QsoMode -- ^ Mode to check for
         -> HamLog (Seq Qso)
findQsos callsign mband mmode = do
  s <- filterCallsign callsign
  let msb = maybe Nothing (\a -> Just $ S.filter (\qso -> band (_qsoFrequency qso) == a) s) mband
      Just s2 = msb <|> Just s
      msm = maybe Nothing (\a -> Just $ S.filter (\qso -> _qsoMode qso == a) s2) mmode
      Just s3 = msm <|> Just s2

  return s3


data Duplicate = Duplicate { duplicateBand :: Bool -- ^ Indicates whether band matches
                           , duplicateMode :: Bool -- ^ Indicates whether mode matches
                           , duplicateQso :: Qso   -- ^ The potential duplicate QSO
                           }


instance Show Duplicate where
  show d = "Met " ++ c ++ " on " ++ show b ++ " using " ++ show m ++ " at " ++ show (_qsoTimeStart q)
    where b = band $ _qsoFrequency q
          m = _qsoMode q
          q = duplicateQso d
          c = T.unpack $ _qsoCallsign q


findDuplicateQsos :: Qso
                  -> HamLog (Seq Duplicate)
findDuplicateQsos qso = do
  let qso' = sanitizeQso qso
      b = band $ _qsoFrequency qso'
      c = _qsoCallsign qso'
      m = _qsoMode qso'
      f a = if _qsoCallsign a == c && (a /= qso') -- Do not return the same QSO itself. FIXME: It may be necessary to use IDs for this.
            then Just (Duplicate { duplicateBand = band (_qsoFrequency a) == b,
                                   duplicateMode = _qsoMode a == m,
                                   duplicateQso = a })
            else Nothing
      catM :: S.Seq (Maybe a) -> S.Seq a
      catM s = fmap fromJust $ S.filter isJust s

  qsos <- getQsoSeq
  return $ catM $ fmap f qsos


-- | Get the FCC information for a given callsign.
lookupFcc :: Text -> HamLog (Maybe FccResult)
lookupFcc a = liftIO $ fccLookup' a


-- | Get the operator's name for a given callsign from the FCC.
lookupFccName :: Text -> HamLog Text
lookupFccName a = liftIO $ fccLookupName a


-- | Make a cabrillo log from the current log, given a cabrillo configuration.
makeCabrillo :: Cabrillo -> HamLog CabrilloLog
makeCabrillo cab = do
    l <- getQsoList
    myCall <- _qsoDefaultCallsign <$> asks _configQsoDefaults
    let l' = Prelude.concatMap toCabrillo l
    return $ CabrilloLog cab l'


-- | Run a CAT action.
cat :: CAT.CAT IO a -> HamLog (Either CAT.CATError (a, [Text]))
cat act = do
  -- FIXME: fromJust may be somewhat dangerous. As it currently is used,
  -- it wil always be set, but that is not guaranteed to remain this way.
  usecat <- gets _stateUseCat
  if usecat
    then do
      radio_interface <- fromJust <$> gets _stateSerialCAT
      conf <- asks _configCat
      let catstate = CAT.defaultState { CAT.stateInterface = radio_interface }
      result <- liftIO $ CAT.runCAT conf catstate act
      case result of
        Left (CAT.CATError t s) -> do
          tell $ ["CAT failed: " <> s <> " - turning off CAT for now."]
          s <- get
          put  $ s { _stateUseCat = False }
        Right a -> tell $ snd a
      return result
    else  return $ Left $ (CAT.CATError CAT.CATErrorGeneric "CAT is turned off.")
