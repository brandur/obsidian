{-# OPTIONS -XTypeSynonymInstances -XGeneralizedNewtypeDeriving #-}

module Obsidian.App (
    App, AppEnv(..), AppT, runApp, output404
    , stdPage
) where

import Control.Applicative
import Control.Monad (ap, liftM)
import Control.Monad.Reader (ReaderT(..), MonadReader)
import Control.Monad.Trans (lift, MonadIO)
import Data.List (intercalate)
import Network.CGI (CGI, CGIT, outputNotFound)
import Network.CGI.Monad (MonadCGI(..))
import Network.CGI.Protocol (CGIResult(..))

import Obsidian.CGI

{- function doc {- hello -} -}

data AppEnv = AppEnv {}

newtype AppT m a = AppT (ReaderT AppEnv (CGIT m) a)
    deriving (Monad, MonadIO, MonadReader AppEnv)

type App = AppT IO

instance Applicative App where
    pure = return
    (<*>) = ap

instance Functor App where
    fmap = liftM

instance MonadCGI App where
    cgiAddHeader n = AppT . lift . cgiAddHeader n
    cgiGet = AppT . lift . cgiGet

runApp :: App CGIResult -> CGI CGIResult
runApp (AppT a) = do
    runReaderT a AppEnv {}

output404 :: [String] -> App CGIResult
output404 s = do outputNotFound $ intercalate "/" s

stdPage :: String -> App CGIResult
stdPage body' = do
    outputText body'
