{-# OPTIONS -XTypeSynonymInstances 
            -XGeneralizedNewtypeDeriving #-}

module Obsidian.App (
    App, AppEnv(..), AppT, getOption, runApp, output404
) where

import Control.Applicative
import Control.Monad        ( ap, liftM )
import Control.Monad.Error  ( runErrorT )
import Control.Monad.Reader ( asks, ReaderT(..), MonadReader )
import Control.Monad.Trans  ( lift, MonadIO )
import Data.ConfigFile      ( ConfigParser, get )
import Data.List            ( intercalate )
import Network.CGI          ( CGI, CGIT, outputNotFound )
import Network.CGI.Monad    ( MonadCGI(..) )

import Obsidian.CGI

{- | Provides a container for entities relevant to the current run of the 
   application. -}
data AppEnv = AppEnv { appCP :: ConfigParser }

{- | App monad is a combination of the CGI and Reader monads. -}
newtype AppT m a = AppT (ReaderT AppEnv (CGIT m) a)
    deriving (Monad, MonadIO, MonadReader AppEnv)

{- | App's CGI monad uses the IO monad. -}
type App = AppT IO

{- | Make the App monad an Applicative Functor so that it will work with 
   formlets. -}
instance Applicative App where
    pure = return
    (<*>) = ap

{- | For formlets. See above. -}
instance Functor App where
    fmap = liftM

{- | Make the App monad an instance of MonadCGI. This is done by defining 
   basic CGI functions. Here we reuse existing methods. -}
instance MonadCGI App where
    cgiAddHeader n = AppT . lift . cgiAddHeader n
    cgiGet = AppT . lift . cgiGet

{- | Creates the Reader environment, and returns the CGIResult from within the 
   App monad to the CGI monad. -}
runApp :: ConfigParser -> App CGIResult -> CGI CGIResult
runApp cp (AppT a) = runReaderT a AppEnv { appCP = cp }

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

{- | Outputs a 404 page. -}
-- @todo: add logging here
output404 :: [String] -> App CGIResult
output404 s = outputNotFound $ intercalate "/" s

