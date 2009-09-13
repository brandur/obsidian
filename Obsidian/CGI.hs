module Obsidian.CGI (
    output', outputJSON, outputText
) where

import Control.Monad.Trans       ( MonadIO )
import Data.ByteString.Lazy.UTF8 ( fromString )
import Network.CGI               ( setHeader )
import Network.CGI.Monad         ( MonadCGI(..) )
import Network.CGI.Protocol      ( CGIResult(..) )
import Text.JSON                 ( JSON, encode, toJSObject )

{- Outputs some string to CGI and encodes it automatically (CGI's output does 
   note encode). -}
output' :: MonadCGI m => String -> m CGIResult
output' = return . CGIOutput . fromString

{- Outputs plain text to CGI. -}
outputText :: (MonadCGI m, MonadIO m) => String -> m CGIResult
outputText s = setHeader "Content-Type" "text/plain; charset=utf-8" >> output' s

{- Outputs JSON from an associative list. -}
outputJSON :: (MonadCGI m, MonadIO m, JSON a) => [(String, a)] -> m CGIResult
outputJSON = outputText . encode . toJSObject

