module Obsidian.Util (
    exceptionM, initLog, trim, 
    -- Log
    alertM, criticalM, debugM, emergencyM, errorM, infoM, noticeM, warningM, 
    -- Printf
    printf
) where

import Control.Exception         ( SomeException )
import Data.Char                 ( isSpace, toUpper )
import Data.List                 ( intercalate )
import System.Log.Logger         ( addHandler, alertM, criticalM, debugM, 
                                   emergencyM, errorM, infoM, noticeM, 
                                   Priority(..), rootLoggerName, setLevel, 
                                   updateGlobalLogger, warningM)
import System.Log.Handler.Simple ( fileHandler )
import Text.Printf               ( printf )

import Obsidian.Config

-- ---------------------------------------------------------------------------

m :: String
m = "Obsidian.Util"

-- ---------------------------------------------------------------------------
-- General
--

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- ---------------------------------------------------------------------------
-- Logging
--

{- | Logs an exception. -}
exceptionM :: String -> SomeException -> IO ()
exceptionM m' ex = errorM m' $ "exception: " ++ (show ex)

{- | Initializes logging facilities given a ConfigParser. The relevant config 
   options are "log/path" and "log/priority". -}
initLog :: ConfigParser -> IO ()
initLog cp = do 
    let path  = getD cp ("log" // "path") "obsidian.log"
        level = toLogLevel $ getD cp ("log" // "level") "warning"
    -- Second arg here (level) appears to do nothing
    h <- fileHandler path level
    updateGlobalLogger rootLoggerName (addHandler h . setLevel level)
    infoM m $ printf "Logging started, path = <%s>, level = <%s>" 
                     path (show level)

{- | Parses a Priority data structure from a string. -}
toLogLevel :: String -> Priority
toLogLevel l = 
    let l'     = map toUpper l
        levels = ["DEBUG", "INFO", "NOTICE", "WARNING", "CRITICAL", "ALERT", 
                  "EMERGENCY"]
    in if l' `elem` levels
       then read l' 
       else error $ printf "Invalid log level <%s>. Legal values are: %s" l 
                           (intercalate ", " levels)

