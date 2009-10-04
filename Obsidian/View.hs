module Obsidian.View (
    contentPage, stdPage
) where

import Text.XHtml.Strict ( (<<), (+++), (!), Html, concatHtml, body, header, 
                           paragraph, showHtml, theclass, thediv, thetitle )

import Obsidian.App
import Obsidian.CGI

contentPage :: String -> [String] -> App CGIResult
contentPage title' body' = do
    stdPage title' $ map (paragraph <<) body'

stdPage :: String -> [Html] -> App CGIResult
stdPage title' body' = do
    setHeader "Content-Type" "text/html; charset=utf-8" 
    let h = header << thetitle << title'
    let b = body << thediv ! [theclass "hel"] << concatHtml body'
    output' $ showHtml $ h +++ b

