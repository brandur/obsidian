module Obsidian.Handlers (
    guardIndex, indexPage
) where

import Control.Monad            ( liftM, mzero )
import Happstack.Server         ( askRq, ok, rqUri, toResponse )
import Safe                     ( lastNote )

import Obsidian.App

-- ---------------------------------------------------------------------------
-- Handlers
--

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

