{-# LANGUAGE OverloadedStrings, RankNTypes, BangPatterns, DataKinds, DeriveGeneric, TypeOperators, FlexibleInstances, ScopedTypeVariables, StandaloneDeriving #-}


module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Ham.ADIF
import Ham.Log as HL
import Ham.Cabrillo
import Ham.CabrilloTypes
import Ham.UI.Brick
import Data.List as L
import qualified Data.Text as T (concat, unpack)
import qualified Data.Sequence as S
import Brick.Main
import qualified Graphics.Vty as V
import Ham.Fcc

import Ham.Contest
import Data.Proxy

import System.Exit
import System.Posix.Files (fileExist)
import System.Console.GetOpt
import System.Environment (getArgs)

import GHC.Generics
import Options.Generic

data Options w = Options { cabrillo :: w ::: Maybe String <?> "Export to cabrillo; argument one of {naqp, fieldday}"
                         , lotw :: w ::: Maybe String <?> "Export to LOTW; argument is the file name."
                         , outfile :: w ::: Maybe String <?> "Output file for cabrillo export."
                         , config :: w ::: Maybe FilePath <?> "hamlog.config file to use"
                         , points :: w ::: Maybe String <?> "Points for a contest, one of {fieldday, naqpcw, naqpssb}" } deriving  Generic

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)


main :: IO ()
main = do
  opts :: Options Unwrapped <- unwrapRecord "hamlog"

  let
      configfilename = maybe "hamlog.config" id $ config opts
      mcabrillo = cabrillo opts
      moutfile = outfile opts
      mlotw = lotw opts
      callsign = ""
      location = ""
      name = ""



  let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

  fe <- fileExist configfilename
  mcfg <- if fe then configFromFile configfilename else return (Just defaultConfig)
  maybe (putStrLn "Could not read config file. Please check the file hamlog.config" >> exitFailure) (const $ putStrLn "Config file read successfully.") mcfg

  let appState' = emptyAppState
  let appState = appState' { logConfig = cfg }
      !cfg = maybe defaultConfig id mcfg


  case points opts of
    Nothing -> return ()
    Just contest -> case contest of
                      "fieldday" ->
                        do
                          let conf = ConfigFieldDay { fdEmergencyPower = True,
                                                      fdPower = FD150W,
                                                      fdMediaPublicity = False,
                                                      fdPublicLocation = False,
                                                      fdPublicInfoTable = False,
                                                      fdFormalMessages = 0,
                                                      fdAlternatePower = False,
                                                      fdOtherBonusPoints = 0 }
                          (p, _) <- evalHamLog cfg emptyLogState $ do { readLog; computePoints (Proxy :: Proxy FieldDay) conf }
                          putStrLn $ "Points total: " ++ show p
                          exitSuccess
                      "naqpcw" ->
                        do
                          (p, _) <- evalHamLog cfg emptyLogState $ do { readLog; computePoints (Proxy :: Proxy NAQPCW) () }
                          putStrLn $ "Points total: " ++ show p
                          exitSuccess
                      "naqpssb" ->
                        do
                          (p, _) <- evalHamLog cfg emptyLogState $ do { readLog; computePoints (Proxy :: Proxy NAQPSSB) () }
                          putStrLn $ "Points total: " ++ show p
                          exitSuccess
                      _ -> putStrLn "Unknown contest for points. Choices are: fieldday, naqpcw, naqpssb" >> exitFailure



  case mcabrillo of
    Nothing -> return ()
    Just cabname -> do

      let mh = mcab >>= \cab -> return (do { readLog; makeCabrillo cab })
          mcab = name_to_cab cabname
          name_to_cab cn = case cn of
            "naqp" -> Just $ naqp callsign location name
            "fieldday" -> Just $ field_day callsign location name
            _ -> Nothing

      case mh of
        Nothing -> print ("Unknown cabrillo type " ++ cabname) >> exitFailure
        Just h -> do
          (c, _) <- evalHamLog cfg emptyLogState h
          maybe (putStrLn $ show c) (\a -> writeFile a $ show c) moutfile
          putStrLn $ unlines [
            "Please edit the Cabrillo output file to add your callsign, name,"
            ,"and edit your points, and any other changes you may want to make."]
          exitSuccess

  case mlotw of
    Nothing -> return ()
    Just adifname -> do
      let act = do
            readLog
            qsos <- getQsoSeq
            let texts = fmap qsoAdif qsos
            return texts
      (texts, _) <- evalHamLog cfg emptyLogState act
      let t = T.unpack $ T.concat $ toList $ S.intersperse "\n" texts
      writeFile adifname t
      exitSuccess


  initialVty <- buildVty
  s' <- customMain initialVty buildVty Nothing app $ appState
  configToFile (logConfig s') configfilename
  return ()
