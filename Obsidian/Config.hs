module Obsidian.Config (
    (//), ConfigParser, getConfig, getD, getF, getDN
) where

import Control.Monad             ( join )
import Control.Monad.Error       ( runErrorT )
import Control.Monad.Trans       ( liftIO )
import Data.Char                 ( isDigit )
import Data.ConfigFile           ( CPError, ConfigParser, emptyCP, get, 
                                   readfile )
import Data.Either.Utils         ( forceEither )
import Safe                      ( lastNote )

-- ---------------------------------------------------------------------------
-- Data
--

{- | Provides a full representation of a configuration option, including both 
   section and the option. This type should be constructed with the (//) 
   operator. -}
data ConfigOption = ConfigOption {
    section :: String, 
    option  :: String
}

instance Show ConfigOption where
    show c = section c ++ "/" ++ option c

-- ---------------------------------------------------------------------------
-- Exports
--

{- | Shortcut for constructing a ConfigOption. -}
(//) :: String -> String -> ConfigOption
sec // opt = ConfigOption sec opt

{- | Gets a ConfigParser built from the file at the given path. -}
getConfig :: String -> IO (Either CPError ConfigParser)
getConfig c = runErrorT . join . liftIO $ readfile emptyCP c

{- | ConfigParser get that allows a default value to be specified for when the 
   given config value is missing. -}
getD :: ConfigParser -> ConfigOption -> String -> String
getD cp c def = do 
    opt <- runErrorT $ get cp (section c) (option c)
    case opt of 
        Left _  -> def
        Right o -> o

{- | ConfigParser get that throws an exception when the given config value is 
   missing. -}
getF :: ConfigParser -> ConfigOption -> String
getF cp c = forceEither $ get cp (section c) (option c)

getDN :: (Read a, Show a) => ConfigParser -> ConfigOption -> a -> a
getDN cp c def = do
    readNumber c $ getD cp c (show def)

-- ---------------------------------------------------------------------------
-- Helpers
--

{- | This method pulled directly from the Gitit source. -}
readNumber :: (Read a) => ConfigOption -> String -> a
readNumber c "" = error $ show c ++ " must be a number."
readNumber c x  =
    let x' = case lastNote "readNumber" x of
                  'K'  -> init x ++ "000"
                  'M'  -> init x ++ "000000"
                  'G'  -> init x ++ "000000000"
                  _    -> x
    in if all isDigit x'
          then read x'
          else error $ show c ++ " must be a number."

