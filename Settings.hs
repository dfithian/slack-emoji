module Settings where

import ClassyPrelude
import Data.Aeson (FromJSON(parseJSON), withObject, (.:), (.:?))
import Data.Time.Clock (NominalDiffTime)
import Network.URI (URI, parseURI)

data AppSettings = AppSettings
  { appWordsApi            :: WordsApiSettings
  , appSynonymValidityTime :: NominalDiffTime
  }

data WordsApiSettings = WordsApiSettings
  { wordsApiToken :: Text
  , wordsApiUri   :: URI
  , wordsApiLimit :: Int
  }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \ obj ->
    AppSettings <$> obj .: "words-api"
                <*> (fromInteger . (*3600) . fromMaybe 2 <$> obj .:? "synonym-validity-time")

instance FromJSON WordsApiSettings where
  parseJSON = withObject "WordsApiSettings" $ \ obj ->
    WordsApiSettings <$> obj .: "token"
                     <*> (maybe (fail "unparseable uri") pure . parseURI =<< obj .: "url")
                     <*> obj .: "limit"
