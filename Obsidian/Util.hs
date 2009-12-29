module Obsidian.Util (
    exceptionM, getConfig, getF, initFileStore, initLog, trim, 
    -- Log
    alertM, criticalM, debugM, emergencyM, errorM, infoM, noticeM, warningM, 
    -- Printf
    printf
) where

import Control.Exception         ( SomeException, throwIO, try )
import Control.Monad             ( join, unless )
import Control.Monad.Error       ( runErrorT )
import Control.Monad.Trans       ( liftIO )
import Data.Char                 ( isSpace, toLower )
import Data.ConfigFile           ( CPError, ConfigParser, emptyCP, get, 
                                   readfile )
import Data.FileStore.Git        ( gitFileStore )
import Data.FileStore.Types      ( FileStore, FileStoreError(..), initialize )
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

{- | Gets a ConfigParser built from the file at the given path. -}
getConfig :: String -> IO (Either CPError ConfigParser)
getConfig c = runErrorT . join . liftIO $ readfile emptyCP c

{- | ConfigParser get that allows a default value to be specified for when the 
   given config value is missing. -}
getD :: ConfigParser -> String -> String -> String -> String
getD cp section option defaultVal = do 
    opt <- runErrorT $ get cp section option
    case opt of 
        Left _  -> defaultVal
        Right o -> o

{- | ConfigParser get that throws an exception when the given config value is 
   missing. -}
getF :: ConfigParser -> String -> String -> String
getF cp section option = forceEither $ get cp section option

-- ---------------------------------------------------------------------------
-- File store
--

{- | Initializes a filestore backend given a ConfigParser. This creates a new 
     repository at "wiki/path" if one didn't exist there before. -}
initFileStore :: ConfigParser -> IO (FileStore)
initFileStore cp = do
    let fsPath = getF cp "file_store" "path"
    infoM m $ printf "filestore path = <%s>" fsPath
    let fs = gitFileStore fsPath
    repoExists <- try (initialize fs) >>= \res ->
        case res of
            Right _               -> do
                warningM m $ printf "Created repository in <%s>" fsPath
                return False
            Left RepositoryExists -> return True
            Left e                -> throwIO e >> return False
    unless repoExists $ do
        infoM m $ "@todo: repo didn't exist, creating default files"
    return fs

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
    let path     = getD cp "log" "path" "obsidian.log"
    let priority = toPriority $ getD cp "log" "priority" "warning"
    -- Second arg here (priority) appears to do nothing
    h <- fileHandler path priority
    updateGlobalLogger rootLoggerName (addHandler h . setLevel priority)
    infoM m $ printf "logging started, path = <%s>, priority = <%s>" 
                     path (show priority)

{- | Parses a Priority data structure from a string. -}
toPriority :: String -> Priority
toPriority p = do
    let p' = map toLower p
    case p' of 
        "debug"     -> DEBUG
        "info"      -> INFO 
        "notice"    -> NOTICE 
        "warning"   -> WARNING
        "error"     -> ERROR
        "critical"  -> CRITICAL
        "alert"     -> ALERT
        "emergency" -> EMERGENCY
        _           -> error $ "bad priority: " ++ p

