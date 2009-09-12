import Codec.Binary.UTF8.String (decodeString)
import Control.Concurrent (forkIO)
import Data.List (find, intercalate)
import Data.List.Split (splitOn)
import Network.CGI (redirect, requestURI, requestMethod)
import Network.CGI.Protocol (CGIResult(..))
import Network.FastCGI (runFastCGIConcurrent')
import Network.URI (URI(..), unEscapeString)

import Obsidian.App

main :: IO()
main = do runFastCGIConcurrent' forkIO 2048 (do 
           runApp handleRequest)

handleRequest :: App CGIResult
handleRequest = do
    uri    <- requestURI
    method <- requestMethod
    dispatch' method (pathList uri)

pathList :: URI -> [String]
pathList = splitOn "/" . decodeString . unEscapeString . uriPath

dispatch' :: String -> [String] -> App CGIResult
dispatch' method path = 
    case path of 
        ["",""] -> frontPage
        ("":xs) -> case find (== "") xs of 
                       Nothing -> dispatch method xs
                       Just _  -> redirect $ '/' : intercalate "/" (filter (/= "") xs)
        _       -> output404 path

dispatch :: String -> [String] -> App CGIResult

dispatch "GET" ["haskell"]           = stdPage "haskell"
dispatch "GET" ["help"]              = stdPage "help"

dispatch _ path = output404 path

frontPage :: App CGIResult
frontPage = do
    stdPage "Hello, world!"
