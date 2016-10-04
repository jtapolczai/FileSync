module System.IO.FileSync.Settings where

import Data.Aeson
import Data.Aeson.Types
import Data.Default
import System.REPL.Config

data AppSettings = AppSettings {
   _appSettingsShowFileActions :: Bool
}

instance Default AppSettings where
   def = AppSettings True

instance ToJSON AppSettings where
   toJSON (AppSettings showFileActions) =
      object ["showFileActions" .= showFileActions]

instance FromJSON AppSettings where
   parseJSON (Object v) = do
      showFileActions <- v .: "showFileActions"
      return $ AppSettings showFileActions
   parseJSON invalid = typeMismatch "Settings" invalid

-- |Tries to read a config file.
--  If the file is not found, a default file is created.
--  If the file is found but malformed, the defaults are returned, but the
--  file is left untouched.
--
--  In either case, an optional warning is returned too.
readAppSettings :: IO (AppSettings, Maybe String)
readAppSettings = do
   x <- readConfigJSON "settings.json"
   case x of
      Just json -> return (json, Nothing)
      Nothing -> return (def, Just "settings.json could not be parsed. Using defaults.")
