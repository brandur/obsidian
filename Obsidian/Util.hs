module Obsidian.Util (
    getConfig, initLog, trim, 
    -- Log
    alertM, criticalM, debugM, emergencyM, errorM, infoM, noticeM, warningM, 
    -- Printf
    printf
) where

import Control.Monad             ( join )
import Control.Monad.Error       ( runErrorT )
import Control.Monad.Trans       ( liftIO )
import Data.Char                 ( isSpace, toLower )
import Data.ConfigFile           ( CPError, ConfigParser, emptyCP, get, 
                                   readfile )
import Data.Either.Utils         ( forceEither )
import System.Log.Logger         ( addHandler, alertM, criticalM, debugM, 
                                   emergencyM, errorM, infoM, noticeM, 
                                   Priority(..), rootLoggerName, setLevel, 
                                   updateGlobalLogger, warningM)
import System.Log.Handler.Simple ( fileHandler )
import Text.Printf               ( printf )

m :: String
m = "Obsidian.Util"

-- ---------------------------------------------------------------------------
-- General
--

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- ---------------------------------------------------------------------------
-- Config
--

-- Gets a config parser (or an error)
getConfig :: String -> IO (Either CPError ConfigParser)
getConfig c = runErrorT $ join $ liftIO $ readfile emptyCP c

-- ---------------------------------------------------------------------------
-- Logging
--

initLog :: ConfigParser -> IO ()
initLog cp = do 
    let path     = forceEither $ get cp "log" "path"
    let priority = toPriority $ forceEither $ get cp "log" "priority"
    -- Second arg here (priority) appears to do nothing
    h <- fileHandler path priority
    updateGlobalLogger rootLoggerName (addHandler h . setLevel priority)
    infoM m $ printf "logging started, path = <%s>, priority = <%s>" 
                     path (show priority)

toPriority :: String -> Priority
toPriority p = do
    let p' = map toLower p
    case p' of 
        "debug"     -> DEBUG
        "info"      -> INFO 
        "notice"    -> WARNING
        "error"     -> ERROR
        "critical"  -> CRITICAL
        "alert"     -> ALERT
        "emergency" -> EMERGENCY
        _           -> error $ "bad priority: " ++ p

