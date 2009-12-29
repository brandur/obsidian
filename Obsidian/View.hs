module Obsidian.View (
    contentPage, goToPage, stdPage
) where

import Control.Exception     ( try )
import Control.Monad         ( liftM )
import Control.Monad.Trans   ( liftIO )
--import qualified Data.ByteString.Lazy as B
import Data.Char             ( toLower )
import Data.FileStore.Types  ( FileStore, FileStoreError(..), index, retrieve 
                             )
import Data.List             ( find )
import System.FilePath.Posix ( dropExtension, takeExtension )
import Text.XHtml.Strict     ( (<<), (+++), (!), Html, concatHtml, body, 
                               header, paragraph, showHtml, theclass, thediv, 
                               thetitle )

import Obsidian.App
import Obsidian.CGI
import Obsidian.Util

m :: String
m = "Obsidian.View"

contentPage :: String -> [String] -> App CGIResult
contentPage title' body' = do
    stdPage title' $ map (paragraph <<) body'

goToPage :: String -> App CGIResult
goToPage path = do
    fs <- getFileStore
    let isPageFile f = ".page" == takeExtension f
    let onlyPages    = map dropExtension . filter isPageFile
    allPageNames <- liftM onlyPages $ liftIO $ index fs
    liftIO $ debugM m $ printf "Going to page = <%s>" path
    let findPage p         = find p allPageNames
    let exactMatch p       = path == p
    let insensitiveMatch p = (map toLower path) == (map toLower p)
    case findPage exactMatch of
        Just p  -> do 
            liftIO $ debugM m $ printf "Found exact match for page"
            renderContent fs p
        Nothing -> do 
            case findPage insensitiveMatch of
                Just p  -> do
                    liftIO $ debugM m $ 
                        printf "Found insensitive match for page = <%s>" p
                    renderContent fs p
                Nothing -> contentPage "not found" ["not found"]

renderContent :: FileStore -> String -> App CGIResult
renderContent fs p = do 
    let p' = p ++ ".page"
    res <- liftIO $ try (retrieve fs p' Nothing)
    case res of 
        Right contents -> contentPage p' [contents]
        Left NotFound  -> contentPage "not found" ["not found: " ++ p']
        Left e         -> error (show e)

stdPage :: String -> [Html] -> App CGIResult
stdPage title' body' = do
    setHeader "Content-Type" "text/html; charset=utf-8" 
    let h = header << thetitle << title'
    let b = body << thediv ! [theclass "hel"] << concatHtml body'
    output' $ showHtml $ h +++ b

