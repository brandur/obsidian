module Obsidian.Article (
) where

import Obsidian.App

{- | Gets an option from the articles section of the config file. -}
getArticleOption :: String -> App (Maybe String)
getArticleOption option = getOption "article"

{- | Gets the path to the Atom XML file containing mutelight.org articles. -}
getArticleXMLPath :: App String
getArticleXMLPath = fromJust <$> getArticleOption "xml_path"

publishedArticles :: App [Article]
publishedArticles = do
    xml  <- readFile getArticleXMLPath
