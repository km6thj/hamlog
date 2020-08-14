import Ham.Cabrillo
import Ham.CabrilloTypes
import Ham.QsoCabrillo
import Ham.Data
import Ham.Log

import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Format
import qualified Data.Sequence as S
import System.Exit
import System.Environment (getArgs)
import qualified Data.Text as T
import Data.Text (Text)


make_cabrillo :: Cabrillo -> HamLog CabrilloLog
make_cabrillo cab = do
    l <- qsoList
    myCall <- _qsoDefaultCallsign <$> asks _configQsoDefaults
    let l' = concatMap toCabrillo l
    return $ CabrilloLog cab l'



main = do
    [configFile] <- getArgs
    config <- configFromFile configFile >>=
                maybe (putStrLn ("config file error loading " ++ configFile) >> exitFailure) return
    let h = do
           readLog
           c <- make_cabrillo cab
           liftIO $ print c
        cab = field_day "KM6THJ" "CA" "Lea"

    evalHamLog config emptyLogState h
    return ()
