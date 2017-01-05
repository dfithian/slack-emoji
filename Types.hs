module Types where

import ClassyPrelude
import Control.Lens.TH (makeLenses)
import Data.CSV.Conduit.Conversion
  ( ToNamedRecord(toNamedRecord), namedRecord, (.=)
  , FromNamedRecord(parseNamedRecord), (.:) )
import Data.Text (splitOn)

-- make this polymorphic in case we ever tried to make it something it's not
type Key a = Int

-- wrap up an entity with its key
data Keyed a = Keyed
  { _key :: Key a
  , _ent :: a
  }

data Entry = Entry
  { _entryKeyword  :: Text
  , _entryEntries  :: [Text]
  } deriving (Eq, Ord, Show)

data Synonym = Synonym
  { _synonymKeyword  :: Text
  , _synonymSynonyms :: [Text]
  , _synonymUpdated  :: UTCTime
  } deriving (Eq, Ord, Show)

deriving instance Generic Entry
deriving instance Generic Synonym

makeLenses ''Keyed
makeLenses ''Entry
makeLenses ''Synonym

instance ToNamedRecord Entry where
  toNamedRecord (Entry keyword entries) = namedRecord ["keyword" .= keyword, "entries" .= intercalate "," entries]
instance FromNamedRecord Entry where
  parseNamedRecord obj = Entry <$> obj .: "keyword" <*> (splitOn "," <$> obj .: "entries")
