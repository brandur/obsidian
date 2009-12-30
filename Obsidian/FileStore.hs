module Obsidian.FileStore (
    FileStoreError(..), initFileStore, index, retrieve
) where

import Control.Exception         ( throwIO, try )
import Control.Monad             ( unless )
import Data.FileStore.Git        ( gitFileStore )
import Data.FileStore.Types      ( FileStore, FileStoreError(..), index, 
                                   initialize, retrieve )

import Obsidian.Config
import Obsidian.Util

m :: String
m = "Obsidian.FileStore"

-- ---------------------------------------------------------------------------
-- File Store
--

{- | Initializes a filestore backend given a ConfigParser. This creates a new 
     repository at "wiki/path" if one didn't exist there before. -}
initFileStore :: ConfigParser -> IO (FileStore)
initFileStore cp = do
    let fsPath = getF cp ("file_store" // "path")
    infoM m $ printf "Filestore path = <%s>" fsPath
    let fs = gitFileStore fsPath
    repoExists <- try (initialize fs) >>= \res ->
        case res of
            Right _               -> do
                warningM m $ printf "Created repository in <%s>" fsPath
                return False
            Left RepositoryExists -> return True
            Left e                -> throwIO e >> return False
    unless repoExists $ do
        infoM m $ "@todo: repo didn't exist, creating default files"
    return fs

