module Main where

import Codec.Binary.UTF8.String ( decodeString )
import Control.Concurrent       ( forkIO )
import Control.Monad            ( join, liftM )
import Control.Monad.Error      ( runErrorT )
import Control.Monad.Trans      ( liftIO )
import Data.ConfigFile          ( CPError, ConfigParser, emptyCP, readfile )
import Data.Either.Utils        ( forceEither )
import Data.List                ( find, intercalate )
import Data.List.Split          ( splitOn )
import Network.CGI              ( redirect, requestURI, requestMethod )
import Network.CGI.Protocol     ( CGIResult(..) )
import Network.FastCGI          ( runFastCGIConcurrent' )
import Network.URI              ( URI(..), unEscapeString )

import Obsidian.App
import Obsidian.View

-- When starting up, start listening for connections immediately. The number 
-- of threads should be the same as what the frontend webserver is configured 
-- with.
main :: IO()
main = do cp <- liftM forceEither getConfig 
          runFastCGIConcurrent' forkIO 2048 $ runApp cp handleRequest

-- Here we just define the config's location
configFile :: String
configFile = "etc/obsidian.conf"

-- Gets a config parser (or an error)
getConfig :: IO (Either CPError ConfigParser)
getConfig = runErrorT $ join $ liftIO $ readfile emptyCP configFile

-- Digest the incoming URI before sending it to the dispatcher
handleRequest :: App CGIResult
handleRequest = do
    uri    <- requestURI
    method <- requestMethod
    dispatch' method (pathList uri)

-- Split a URI into a list of its components
pathList :: URI -> [String]
pathList = splitOn "/" . decodeString . unEscapeString . uriPath

-- Pre-dispatch. Handles dispatching to the index page and removing trailing 
-- slashes.
dispatch' :: String -> [String] -> App CGIResult
dispatch' method path = 
    case path of 
        ["",""] -> indexPage
        ("":xs) -> case find (== "") xs of 
                       Nothing -> dispatch method xs
                       -- Redirect a trailing slash back to the same resource 
                       -- minus the slash
                       Just _  -> redirect $ '/' : intercalate "/" (filter (/= "") xs)
        _       -> output404 path

-- Signature for the dispatcher. Its job is to send each request to an 
-- appropriate module.
dispatch :: String -> [String] -> App CGIResult

dispatch "GET" ["haskell"]           = contentPage "haskell" ["haskell", "is tricky"]
--dispatch "GET" ["help"]              = stdPage "help"

dispatch _ path = output404 path

indexPage :: App CGIResult
indexPage = do
    contentPage "hello" ["Hello, world!"]

