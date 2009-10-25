module Obsidian.CGI (
    CGIResult(..), output', outputJSON, outputText, setHeader, tryCGI'
) where

import Control.Arrow             ( first )
import Control.Exception         ( Exception, try )
import Control.Monad             ( liftM )
import Control.Monad.Reader      ( ReaderT(..) )
import Control.Monad.Writer      ( WriterT(..) )
import Control.Monad.Trans       ( MonadIO )
import Data.ByteString.Lazy.UTF8 ( fromString )
import Data.Monoid               ( mempty )
import Network.CGI               ( setHeader )
import Network.CGI.Monad         ( CGI, CGIT(..), MonadCGI(..) )
import Network.CGI.Protocol      ( CGIResult(..) )
import Text.JSON                 ( JSON, encode, toJSObject )

{- | Outputs some string to CGI and encodes it automatically (CGI's output 
   does note encode). -}
output' :: MonadCGI m => String -> m CGIResult
output' = return . CGIOutput . fromString

{- | Outputs plain text to CGI. -}
outputText :: (MonadCGI m, MonadIO m) => String -> m CGIResult
outputText s = do
    setHeader "Content-Type" "text/plain; charset=utf-8" 
    output' s

{- | Outputs JSON from an associative list. -}
outputJSON :: (MonadCGI m, MonadIO m, JSON a) => [(String, a)] -> m CGIResult
outputJSON = outputText . encode . toJSObject

-- 
-- Exceptions
-- 

tryCGI' :: Exception e => CGI a -> CGI (Either e a)
tryCGI' (CGIT c) = CGIT (ReaderT (WriterT . f . runWriterT . runReaderT c ))
    where
      f = liftM (either (\ex -> (Left ex, mempty)) (first Right)) . try
