module Main where

import Control.Monad            ( liftM, mplus, msum )
import Control.Monad.Reader     ( ReaderT(..) )
import Control.Monad.Trans      ( liftIO )
import Data.Either.Utils        ( forceEither )
import Data.FileStore.Types     ( FileStore )
import Happstack.Server         ( Conf(..), FilterFun, ServerMonad, 
                                  ServerPart, Response, 
                                  fileServeStrict, mapServerPartT, 
                                  simpleHTTP, nullConf, setHeader )

import Obsidian.App
import Obsidian.Config
import Obsidian.Handlers
import Obsidian.Util

-- ---------------------------------------------------------------------------
-- Main
--

main :: IO ()
main = do
    cp <- liftM forceEither $ getConfig configFile
    liftIO $ initLog cp
    let p = getDN cp ("http" // "port") 3001
    fs <- liftIO $ initFileStore cp
    simpleHTTP nullConf { port = p } $ runObsidian cp fs

-- ---------------------------------------------------------------------------
-- Helpers
--

-- Here we just define the config's location
configFile :: String
configFile = "etc/obsidian.conf"

runObsidian :: ConfigParser -> FileStore -> ServerPart Response
runObsidian cp fs = 
    let static        = getD cp ("http" // "static_path") "data/static"
        staticHandler = withExpiresHeaders $ fileServeStrict [] static
        env           = AppEnv { appCP = cp, appFS = fs }
        handlers      = obsidianHandlers
    in staticHandler `mplus` runHandler env (msum handlers)

-- All handlers for the Obsidian application
obsidianHandlers :: [Handler]
obsidianHandlers = 
    [
    indexPage
    ]

-- Converts an Obsidian Handler into a standard Happstack ServerPart.
runHandler :: AppEnv -> Handler -> ServerPart Response
runHandler = mapServerPartT . unpackReaderT

unpackReaderT:: (Monad m)
    => c
    -> (ReaderT c m) (Maybe ((Either b a), FilterFun b))
    -> m (Maybe ((Either b a), FilterFun b))
unpackReaderT st handler = runReaderT handler st

withExpiresHeaders :: ServerMonad m => m Response -> m Response
withExpiresHeaders = liftM (setHeader "Cache-Control" "max-age=21600")

