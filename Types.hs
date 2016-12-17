module Types where

import ClassyPrelude
import Control.Lens.TH (makeLenses)
import Data.CSV.Conduit.Conversion
  ( ToNamedRecord(toNamedRecord), namedRecord, (.=)
  , FromNamedRecord(parseNamedRecord), (.:) )
import Data.Text (splitOn)
import TextShow (showt)
import TextShow.TH (deriveTextShow)

-- make this polymorphic in case we ever tried to make it something it's not
type Key a = Int

-- wrap up an entity with its key
data Keyed a = Keyed
  { _key :: Key a
  , _ent :: a
  }

data Entry = Entry
  { _entryKeyword :: Text
  , _entryEntries :: [Text]
  }

deriving instance Eq Entry

deriving instance Generic Entry

makeLenses ''Keyed
makeLenses ''Entry

deriveTextShow ''Entry

instance Show Entry where show = unpack . showt

instance ToNamedRecord Entry where
  toNamedRecord (Entry keyword entries) = namedRecord ["keyword" .= keyword, "entries" .= intercalate "," entries]
instance FromNamedRecord Entry where
  parseNamedRecord obj = Entry <$> obj .: "keyword" <*> (splitOn "," <$> obj .: "entries")
