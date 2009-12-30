module Obsidian.App (
    AppEnv(..), Handler, ObsidianServerPart, getFileStore
) where

import Control.Monad        ( liftM )
import Control.Monad.Reader ( ReaderT(..), ask )
import Data.ConfigFile      ( ConfigParser )
import Data.FileStore.Types ( FileStore )
import Happstack.Server     ( ServerPartT(..), Response )

-- ---------------------------------------------------------------------------
-- Monad
--

{- | Provides a container for entities relevant to the current run of the 
   application. -}
data AppEnv = AppEnv { 
      appCP :: ConfigParser 
    , appFS :: FileStore 
    }

{- | ServerPartT is a response builder. Here we define a type that will allow 
   us to use all of ServerPartT's functionality, and we transform the monad 
   with ReaderT so that we can store some additional application data. -}
type ObsidianServerPart = ServerPartT (ReaderT AppEnv IO)

{- | Handler is a type alias for returning some Response data from inside the 
   ObsidianServerPart monad. -}
type Handler = ObsidianServerPart Response

-- ---------------------------------------------------------------------------
-- App-bound data accessors
--

{- | Gets the FileStore bound to the ObsidianServerPart monad. -}
getFileStore :: ObsidianServerPart FileStore
getFileStore = liftM appFS ask

