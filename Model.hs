module Model where

import ClassyPrelude
import Control.Lens (Iso', from, iso, to, view)
import Database.Persist (Entity(Entity), entityKey, entityVal)
import Database.Persist.Quasi (upperCaseSettings)
import Database.Persist.TH (mkMigrate, mkPersist, mpsGenerateLenses, mpsGeneric, persistFileWith, share, sqlSettings)
import qualified Types as T

let settings = sqlSettings { mpsGenerateLenses = True, mpsGeneric = False }
 in share [mkPersist settings, mkMigrate "migrateAll"]
    $(persistFileWith upperCaseSettings "models")

keyedEntryIso :: Iso' (T.Keyed T.Entry) (Entity EntryDB)
keyedEntryIso = iso toDb toApi
  where
    toDb = do
      ent <- view (T.ent . entryIso)
      k <- view (T.key . to EntryDBKey)
      pure $ Entity k ent
    toApi = do
      ent <- view (to entityVal . from entryIso)
      k <- view (to entityKey . to unEntryDBKey)
      pure $ T.Keyed k ent

entryIso :: Iso' T.Entry EntryDB
entryIso = iso toDb toApi
  where
    toDb = EntryDB
      <$> view T.entryKeyword
      <*> view T.entryEntries
    toApi = T.Entry
      <$> view entryDBKeyword
      <*> view entryDBEntries
