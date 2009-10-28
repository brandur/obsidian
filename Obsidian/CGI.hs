module Obsidian.CGI (
    CGIResult(..), catchCGI', handleErrors', output', outputJSON, outputText, 
    setHeader, tryCGI'
) where

import Control.Arrow             ( first )
import Control.Exception         ( Exception, SomeException, try )
import Control.Monad             ( liftM )
import Control.Monad.Reader      ( ReaderT(..) )
import Control.Monad.Writer      ( WriterT(..) )
import Control.Monad.Trans       ( MonadIO, liftIO )
import Data.ByteString.Lazy.UTF8 ( fromString )
import Data.Monoid               ( mempty )
import Network.CGI               ( outputInternalServerError, setHeader )
import Network.CGI.Monad         ( CGI, CGIT(..), MonadCGI(..) )
import Network.CGI.Protocol      ( CGIResult(..) )
import Text.JSON                 ( JSON, encode, toJSObject )

import Obsidian.Util

-- ---------------------------------------------------------------------------

m :: String
m = "Obsidian.CGI"

-- ---------------------------------------------------------------------------
-- Output
--

{- | Outputs some string to CGI and encodes it automatically (CGI's output 
   does note encode). -}
output' :: MonadCGI m => String -> m CGIResult
output' = return . CGIOutput . fromString

{- | Logs an exception and outputs a 500 internal server error. -}
outputException' ::  (MonadCGI m, MonadIO m) => SomeException -> m CGIResult
outputException' ex = do
    liftIO $ exceptionM m ex
    outputInternalServerError [show ex]

{- | Outputs plain text to CGI. -}
outputText :: (MonadCGI m, MonadIO m) => String -> m CGIResult
outputText s = do
    setHeader "Content-Type" "text/plain; charset=utf-8" 
    output' s

{- | Outputs JSON from an associative list. -}
outputJSON :: (MonadCGI m, MonadIO m, JSON a) => [(String, a)] -> m CGIResult
outputJSON = outputText . encode . toJSObject

-- ---------------------------------------------------------------------------
-- Exceptions
--

{- | Catches an exception thrown from the CGI monad if necessary and passes 
   the exception to an apprioriate function for processing. -}
handleErrors' :: CGI CGIResult -> CGI CGIResult
handleErrors' a = catchCGI' (do 
                      r <- a
                      return r)
                  (outputException')

{- | Handles a thrown exception in the CGI monad. If an exception is thrown, 
   it's passed to the given handler method to be taken care of. -}
catchCGI' :: Exception e => CGI a -> (e -> CGI a) -> CGI a
catchCGI' c h = tryCGI' c >>= either h return

tryCGI' :: Exception e => CGI a -> CGI (Either e a)
tryCGI' (CGIT c) = CGIT (ReaderT (WriterT . f . runWriterT . runReaderT c ))
    where
        f = liftM (either (\ex -> (Left ex, mempty)) (first Right)) . try

