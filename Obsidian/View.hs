module Obsidian.View (
    contentPage, stdPage, couchPage
) where

import Text.XHtml.Strict   ( (<<), (+++), (!), Html, concatHtml, body, header, 
                             paragraph, showHtml, theclass, thediv, thetitle )

import Obsidian.App
import Obsidian.Article
import Obsidian.CGI

couchPage :: App CGIResult
couchPage = do
    (d, r) <- newDoc' (Article "Test Article" "Test article's body.")
    contentPage "inserted" [ "inserted random_doc", (show d), (show r) ]

contentPage :: String -> [String] -> App CGIResult
contentPage title' body' = do
    stdPage title' $ map (paragraph <<) body'

stdPage :: String -> [Html] -> App CGIResult
stdPage title' body' = do
    setHeader "Content-Type" "text/html; charset=utf-8" 
    let h = header << thetitle << title'
    let b = body << thediv ! [theclass "hel"] << concatHtml body'
    output' $ showHtml $ h +++ b

