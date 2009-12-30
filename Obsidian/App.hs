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

type ObsidianServerPart = ServerPartT (ReaderT AppEnv IO)

type Handler = ObsidianServerPart Response

-- ---------------------------------------------------------------------------
-- App-bound data accessors
--

getFileStore :: ObsidianServerPart FileStore
getFileStore = liftM appFS ask

{-
{-# OPTIONS -XTypeSynonymInstances 
            -XGeneralizedNewtypeDeriving 
            -XScopedTypeVariables #-}

module Obsidian.App (
    App, AppEnv(..), AppT, getFileStore, getOption, output404, runApp, tryApp, 
) where

import Prelude              hiding ( catch )
import Control.Applicative  ( (<*>), Applicative, pure )
import Control.Exception    ( SomeException )
import Control.Monad        ( ap, liftM )
import Control.Monad.Error  ( runErrorT )
import Control.Monad.Reader ( asks, ReaderT(..), MonadReader )
import Control.Monad.Trans  ( MonadIO, lift, liftIO )
import Data.ConfigFile      ( ConfigParser, get )
import Data.FileStore.Types ( FileStore )
import Data.List            ( intercalate )
import Network.CGI          ( CGI, CGIT, outputNotFound )
import Network.CGI.Monad    ( MonadCGI(..) )

import Obsidian.CGI
import Obsidian.Util

-- ---------------------------------------------------------------------------

m :: String
m = "Obisidian.App"

-- ---------------------------------------------------------------------------
-- Monad
--

{- | Provides a container for entities relevant to the current run of the 
   application. -}
data AppEnv = AppEnv { appCP :: ConfigParser, 
                       appFS :: FileStore }

{- | App monad is a combination of the CGI and Reader monads. -}
newtype AppT m a = AppT (ReaderT AppEnv (CGIT m) a)
    deriving (Monad, MonadIO, MonadReader AppEnv)

{- | App's CGI monad uses the IO monad. -}
type App = AppT IO

{- | Make the App monad an Applicative Functor so that it will work with 
   formlets. -}
instance Applicative App where
    pure  = return
    (<*>) = ap

{- | For formlets. See above. -}
instance Functor App where
    fmap = liftM

{- | Make the App monad an instance of MonadCGI. This is done by defining 
   basic CGI functions. Here we reuse existing methods. -}
instance MonadCGI App where
    cgiAddHeader n = AppT . lift . cgiAddHeader n
    cgiGet = AppT . lift . cgiGet

-- ---------------------------------------------------------------------------
-- Entry-point
--

{- | Creates the Reader environment, and returns the CGIResult from within the 
   App monad to the CGI monad. -}
runApp :: ConfigParser -> FileStore -> App CGIResult -> CGI CGIResult
runApp cp fs (AppT a) = do
    runReaderT a AppEnv { appCP = cp, 
                          appFS = fs }

-- ---------------------------------------------------------------------------
-- Exceptions
--

tryApp :: App a -> App (Either SomeException a)
tryApp (AppT c) = AppT (ReaderT (tryCGI' . runReaderT c))

-- ---------------------------------------------------------------------------
-- App-bound data accessors
--

{- | Pulls the application's FileStore out of the App monad. -}
getFileStore :: App (FileStore)
getFileStore = do
    fs <- asks appFS
    return fs

{- | Pulls the config option identified by section/option from the App monad's 
   configuration parser. -}
getOption :: String -> String -> App (Maybe String)
getOption section option = do
    cp  <- asks appCP
    opt <- runErrorT $ get cp section option
    case opt of
        -- @todo: log this error
        Left _  -> do
            error $ "no config option: " ++ section ++ "/" ++ option
        Right o -> return $ Just o

-- ---------------------------------------------------------------------------
-- Output
--

{- | Outputs a 404 page and logs the requested resource. -}
output404 :: [String] -> App CGIResult
output404 s = do 
    let p = "404: " ++ intercalate "/" s
    liftIO $ infoM m $ printf "sent 404, requested path was <%s>" p
    outputNotFound p
-}
