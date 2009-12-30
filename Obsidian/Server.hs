module Obsidian.Server (
    setContentType
) where

import Happstack.Server          ( Response, setHeader )

-- ---------------------------------------------------------------------------
-- Server
--

{- Shortcut for setting the Content-Type HTTP header. -}
setContentType :: String -> Response -> Response
setContentType = setHeader "Content-Type"

