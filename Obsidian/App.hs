{-# OPTIONS -XTypeSynonymInstances 
            -XGeneralizedNewtypeDeriving 
            -XScopedTypeVariables #-}

module Obsidian.App (
    App, AppEnv(..), AppT, getOption, newDoc', output404, runApp, tryApp, 
    -- CouchDB
    Doc, Rev
) where

import Prelude              hiding ( catch )
import Control.Applicative  ( (<*>), Applicative, pure )
import Control.Exception    ( ErrorCall, SomeException, catch )
import Control.Monad        ( ap, liftM )
import Control.Monad.Error  ( runErrorT )
import Control.Monad.Reader ( asks, ReaderT(..), MonadReader )
import Control.Monad.Trans  ( MonadIO, lift, liftIO )
import Data.ConfigFile      ( ConfigParser, get )
import Data.Either.Utils    ( forceEither )
import Data.List            ( intercalate )
import Database.CouchDB     ( DB, Doc, Rev, createDB, db, newDoc, runCouchDB' )
import Network.CGI          ( CGI, CGIT, outputNotFound )
import Network.CGI.Monad    ( MonadCGI(..) )
import Text.JSON            ( JSON )

import Obsidian.CGI
import Obsidian.Util

-- ---------------------------------------------------------------------------
-- Monad
--

{- | Provides a container for entities relevant to the current run of the 
   application. -}
data AppEnv = AppEnv { appCP :: ConfigParser,
                       appDB :: DB  }

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
runApp :: ConfigParser -> App CGIResult -> CGI CGIResult
runApp cp (AppT a) = do
    let dbName = forceEither $ get cp "db" "name"
    -- We'd like to create our database if it doesn't exist. Unfortunately, 
    -- the CouchDB lib doesn't seem to provide a way to check whether a 
    -- database exists already (I think this is an API flaw), so we just have 
    -- to try and create it, and catch an exception if one is thrown.
    liftIO $ runCouchDB' (createDB dbName) `catch` \(_ :: ErrorCall) -> return ()
    runReaderT a AppEnv { appCP = cp, 
                          appDB = db dbName }

-- ---------------------------------------------------------------------------
-- Exceptions
--

tryApp :: App a -> App (Either SomeException a)
tryApp (AppT c) = AppT (ReaderT (tryCGI' . runReaderT c))

-- ---------------------------------------------------------------------------
-- Configuration
--

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
-- Database 
--

-- @todo: error handling
newDoc' :: (JSON a) => a -> App (Doc, Rev)
newDoc' body = do
    db' <- asks appDB
    (d, r) <- liftIO $ runCouchDB' $ newDoc db' body
    return $ (d, r)

-- ---------------------------------------------------------------------------
-- Output
--

{- | Outputs a 404 page. -}
-- @todo: add logging here
output404 :: [String] -> App CGIResult
output404 s = outputNotFound $ intercalate "/" s

