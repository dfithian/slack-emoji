module Settings where

import ClassyPrelude
import Data.Aeson (FromJSON(parseJSON), withObject, (.:))
import Network.Wai.Handler.Warp (HostPreference)

data AppSettings = AppSettings
  { appRoot         :: Text
  , appHost         :: HostPreference
  , appPort         :: Int
  }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \ o -> do
    appRoot         <- o .: "approot"
    appHost         <- fromString <$> o .: "host"
    appPort         <- o .: "port"

    pure $ AppSettings {..}
