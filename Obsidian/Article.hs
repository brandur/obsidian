module Obsidian.Article (
    Article(..)
) where

import Database.CouchDB.JSON ( jsonField, jsonObject )
import Text.JSON             ( JSValue(..), JSON, readJSON, showJSON, toJSObject)

data Article = Article
    { articleTitle       :: String
    , articleBody        :: String
    } deriving (Eq,Show)
 
instance JSON Article where
 
    showJSON (Article title body) = JSObject $ toJSObject
        [ ("title", showJSON title)
        , ("body", showJSON body)
        ]
 
    readJSON val = do
        obj         <- jsonObject val
        title       <- jsonField "title" obj
        body        <- jsonField "body" obj
        return (Article title body)

