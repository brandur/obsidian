module Obsidian.Handlers (
    indexPage, handleAny, handlePage, 
    guardIndex
) where

import Control.Exception        ( try )
import Control.Monad            ( liftM, mzero )
import Control.Monad.Trans      ( liftIO )
import qualified Data.ByteString.Lazy as B
import Data.Char                ( toLower )
import Data.List                ( find )
import Happstack.Server         ( anyRequest, askRq, ok, rqUri, 
                                  rsBody, toResponse, uriRest )
import Safe                     ( lastNote )
import System.FilePath.Posix    ( dropExtension, takeExtension )
import Text.XHtml               ( noHtml )

import Obsidian.App
import Obsidian.FileStore
import Obsidian.Util

-- ---------------------------------------------------------------------------

m :: String
m = "Obsidian.Handlers"

-- ---------------------------------------------------------------------------
-- Handlers
--

{- | Handles an exact request for any file in our filestore. Tries to set the 
   response's content-type to one appropriate for the file's extension. -}
handleAny :: Handler
handleAny = uriRest $ \uri -> do
    let path' = uriPath uri
    fs <- getFileStore
    --mimetype <- 
    res <- liftIO $ try (retrieve fs path' Nothing :: IO B.ByteString)
    case res of 
        Right content -> ok $ --setContentType mimetype $ 
            -- The GitIt guy says this is an ugly hack, I'll take his word for 
            -- it
            (toResponse noHtml) { rsBody = content }
        Left NotFound -> anyRequest mzero
        Left e        -> error $ show e

{- | Handles a request for a page file using the request URI and adding a 
   @.page@ extension. If an exact match can't be found, an insensitive match 
   is attempted. -}
handlePage :: Handler
handlePage = uriRest $ \uri -> do
    let path' = uriPath uri
    fs  <- getFileStore
    res <- liftIO $ try $ retrieve fs (path' ++ ".page") Nothing
    case res of 
        Right content -> renderPage content
        Left NotFound -> do
            let isPageFile f = ".page" == takeExtension f
                onlyPages    = map dropExtension . filter isPageFile
            allPageNames <- liftM onlyPages $ liftIO $ index fs
            let findPage p         = find p allPageNames
                insensitiveMatch p = (map toLower path') == (map toLower p)
            case findPage insensitiveMatch of
                Just p  -> do
                    liftIO $ debugM m $ 
                        printf "Found insensitive match for page = <%s>" p
                    content <- liftIO $ retrieve fs (p ++ ".page") Nothing
                    renderPage content
                -- No match so pass through to the next handler
                Nothing -> anyRequest mzero
        Left e -> error $ show e

{- | Handles a request for the front page. -}
indexPage :: Handler
indexPage = ok $ toResponse $ "Hello, world!"

-- ---------------------------------------------------------------------------
-- Guards
--

{- | Succeeds if path is an index path:  e.g. @\/foo\/bar/@. -}
guardIndex :: ObsidianServerPart ()
guardIndex = do
    uri' <- liftM rqUri askRq
    if lastNote "guardIndex" uri' == '/'
        then return ()
        else mzero

-- ---------------------------------------------------------------------------
-- Helpers
--

-- @todo: should show HTML
renderPage :: String -> Handler
renderPage = ok . toResponse

uriPath :: String -> String
uriPath = unwords . words . drop 1 . takeWhile (/='?')


