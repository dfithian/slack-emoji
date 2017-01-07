module Settings where

import ClassyPrelude
import Data.Aeson (FromJSON(parseJSON), withObject, (.:), (.:?))
import Data.Time.Clock (NominalDiffTime)
import Network.URI (URI, parseURI)

data AppSettings = AppSettings
  { appWordsApi               :: WordsApiSettings -- ^ Settings for the words api
  , appSynonymValidityTime    :: NominalDiffTime  -- ^ Time that a synonym is considered valid (in days)
  , appSynonymRefreshInterval :: NominalDiffTime  -- ^ Time between checking for new synonyms in the words api (in hours)
  , appMaxDbConnections       :: Int              -- ^ Maximum database connections
  , appEntriesSeedCsv         :: FilePath         -- ^ Location of seed CSV file
  }

data WordsApiSettings = WordsApiSettings
  { wordsApiToken :: Text
  , wordsApiUri   :: URI
  , wordsApiLimit :: Int
  }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \ obj ->
    AppSettings <$> obj .: "words-api"
                <*> (fromInteger . (*86400) . fromMaybe 2 <$> obj .:? "synonym-validity-time")
                -- <*> (fromInteger . (*3600) . fromMaybe 2 <$> obj .:? "synonym-refresh-interval")
                <*> (fromInteger . fromMaybe 2 <$> obj .:? "synonym-refresh-interval")
                <*> (fromMaybe 10 <$> obj .:? "max-db-connections")
                <*> (fromMaybe "entries.csv" <$> obj .:? "entries-seed-csv")

instance FromJSON WordsApiSettings where
  parseJSON = withObject "WordsApiSettings" $ \ obj ->
    WordsApiSettings <$> obj .: "token"
                     <*> (maybe (fail "unparseable uri") pure . parseURI =<< obj .: "url")
                     <*> obj .: "limit"
