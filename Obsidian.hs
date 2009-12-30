module Main where

import Control.Monad            ( liftM, mplus, msum, mzero )
import Control.Monad.Reader     ( ReaderT(..) )
import Control.Monad.Trans      ( liftIO )
import Data.Either.Utils        ( forceEither )
import Data.FileStore.Types     ( FileStore )
import Happstack.Server         ( Conf(..), FilterFun, ServerMonad, 
                                  ServerPart, Response, askRq, 
                                  fileServeStrict, mapServerPartT, rqUri, 
                                  rsCode, simpleHTTP, nullConf, 
                                  setHeader )
import Safe                     ( lastNote )

import Obsidian.App
import Obsidian.Config
import Obsidian.FileStore
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
-- Core
--

-- Here we just define the config's location
configFile :: String
configFile = "etc/obsidian.conf"

runObsidian :: ConfigParser -> FileStore -> ServerPart Response
runObsidian cp fs = 
    let static        = getD cp ("http" // "static_path") "data/static"
        staticHandler = withLongExpiresHeaders $ fileServeStrict' [] static
        env           = AppEnv { appCP = cp, appFS = fs }
        handlers      = obsidianHandlers
    in staticHandler `mplus` runHandler env (msum handlers)

-- All handlers for the Obsidian application
obsidianHandlers :: [Handler]
obsidianHandlers = [
      guardIndex >> indexPage
    , handlePage
    , handleAny
    ]

-- ---------------------------------------------------------------------------
-- Helpers
--

-- Like 'fileServeStrict', but if file is not found, fail instead of returning 
-- a 404 error.
fileServeStrict' :: [FilePath] -> FilePath -> ServerPart Response
fileServeStrict' ps p = do
    rq <- askRq
    resp <- fileServeStrict ps p
    if rsCode resp == 404 || lastNote "fileServeStrict'" (rqUri rq) == '/'
        then mzero  -- pass through if not found or directory index
        else return resp

-- Converts an Obsidian Handler into a standard Happstack ServerPart.
runHandler :: AppEnv -> Handler -> ServerPart Response
runHandler = mapServerPartT . unpackReaderT

-- Some sort of Haskell voodoo. Copied from Gitit.
unpackReaderT:: (Monad m)
    => c
    -> (ReaderT c m) (Maybe ((Either b a), FilterFun b))
    -> m (Maybe ((Either b a), FilterFun b))
unpackReaderT env handler = runReaderT handler env

-- Sets long expire headers for static files
withLongExpiresHeaders :: ServerMonad m => m Response -> m Response
withLongExpiresHeaders = liftM (setHeader "Cache-Control" "max-age=21600")

