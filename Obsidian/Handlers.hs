module Obsidian.Handlers (
    indexPage
) where

import Happstack.Server         ( ok, toResponse )

import Obsidian.App

-- ---------------------------------------------------------------------------
-- Handlers
--

indexPage :: Handler
indexPage = ok $ toResponse $ "Hello, world!"

