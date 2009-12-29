module Main where

import Codec.Binary.UTF8.String ( decodeString )
import Control.Concurrent       ( forkIO )
import Control.Monad            ( liftM )
import Control.Monad.Trans      ( liftIO )
import Data.Either.Utils        ( forceEither )
import Data.List                ( find, intercalate )
import Data.List.Split          ( splitOn )
import Network.CGI              ( redirect, requestURI, requestMethod )
import Network.FastCGI          ( runFastCGIConcurrent' )
import Network.URI              ( URI(..), unEscapeString )

import Obsidian.App
import Obsidian.CGI
import Obsidian.Util
import Obsidian.View

m :: String
m = "Main"

-- When starting up, start listening for connections immediately. The number 
-- of threads should be the same as what the frontend webserver is configured 
-- with.
main :: IO ()
main = do 
    cp <- liftM forceEither $ getConfig configFile
    liftIO $ initLog cp
    fs <- liftIO $ initFileStore cp
    -- Fork (with lightweight threads) and run the application
    runFastCGIConcurrent' forkIO 2048 $ do 
        handleErrors' $ runApp cp fs handleRequest

-- Here we just define the config's location
configFile :: String
configFile = "etc/obsidian.conf"

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
        ["",""] -> dispatch method ["index"]
        ("":xs) -> case find (== "") xs of 
                       Nothing -> dispatch method xs
                       -- Redirect a trailing slash back to the same resource 
                       -- minus the slash
                       Just _  -> redirect $ 
                           '/':intercalate "/" (filter (/= "") xs)
        _       -> output404 path

-- Signature for the dispatcher. Its job is to send each request to an 
-- appropriate module.
dispatch :: String -> [String] -> App CGIResult

dispatch "GET" ["new", "haskell"]   = contentPage "haskell" ["haskell", "is tricky"]

dispatch "GET" ("new":xs) = goToPage (intercalate "/" xs)

dispatch _ path = output404 path

indexPage :: App CGIResult
indexPage = do
    contentPage "hello" ["Hello, world!"]

