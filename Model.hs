module Model where

import ClassyPrelude
import Control.Lens (Iso', _Just, from, iso, over, preview, to, view)
import Conduit (mapM_C, runResourceT, sourceFileBS)
import Data.Conduit (connect, fuse)
import Data.CSV.Conduit (defCSVSettings, intoCSV)
import Data.CSV.Conduit.Conversion (Named(Named))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.List (nub)
import Database.Persist (Entity(Entity), Key, PersistEntity, count, entityKey, entityVal, insert_, repsert, selectFirst, (==.))
import Database.Persist.Quasi (upperCaseSettings)
import Database.Persist.Sql (runMigration)
import Database.Persist.TH (mkMigrate, mkPersist, mpsGenerateLenses, mpsGeneric, persistFileWith, share, sqlSettings)
import Foundation (App(appSettings), runDb)
import Settings (appEntriesSeedCsv)
import qualified Types as T

let settings = sqlSettings { mpsGenerateLenses = True, mpsGeneric = False }
 in share [mkPersist settings, mkMigrate "migrateAll"]
    $(persistFileWith upperCaseSettings "models")

seedDb :: (MonadBaseControl IO m, MonadIO m, MonadReader App m, MonadThrow m) => m ()
seedDb = do
  entriesSeedCsv <- asks (appEntriesSeedCsv . appSettings)
  runResourceT . runDb $ do
    -- create tables and wipe them
    runMigration migrateAll

    -- seed entry table from csv and initialize synonym table
    sourceFileBS entriesSeedCsv
      `fuse` intoCSV defCSVSettings
      `connect` mapM_C ( \ (Named new) -> do
                           let keyword = view T.entryKeyword new
                           void . whenM ((< 1) <$> count [SynonymDBKeyword ==. keyword]) $
                             insert_ . view synonymIso . T.Synonym keyword [] . posixSecondsToUTCTime . fromInteger $ 0
                           prevMay <- preview (_Just . from keyedEntryIso) <$> selectFirst [EntryDBKeyword ==. keyword] []
                           case prevMay of
                             Just prev -> -- merge existing entries
                               let replacement = over (T.ent . T.entryEntries) (nub . (<> view T.entryEntries new)) prev
                                   (replacementKey, replacementEntity) = entityKey &&& entityVal $ view keyedEntryIso replacement
                               in repsert replacementKey replacementEntity
                             Nothing -> insert_ $ view entryIso new
                       )

keyedIso :: (PersistEntity db)
         => Iso' api db
         -> (T.Key api -> Key db)
         -> (Key db -> T.Key api)
         -> Iso' (T.Keyed api) (Entity db)
keyedIso entityIso toKeyDb toKeyApi = iso toDb toApi
  where
    toDb = do
      ent <- view (T.ent . entityIso)
      k <- view (T.key . to toKeyDb)
      pure $ Entity k ent
    toApi = do
      ent <- view (to entityVal . from entityIso)
      k <- view (to entityKey . to toKeyApi)
      pure $ T.Keyed k ent

keyedEntryIso :: Iso' (T.Keyed T.Entry) (Entity EntryDB)
keyedEntryIso = keyedIso entryIso EntryDBKey unEntryDBKey

entryIso :: Iso' T.Entry EntryDB
entryIso = iso toDb toApi
  where
    toDb = EntryDB
      <$> view T.entryKeyword
      <*> view T.entryEntries
    toApi = T.Entry
      <$> view entryDBKeyword
      <*> view entryDBEntries

keyedSynonymIso :: Iso' (T.Keyed T.Synonym) (Entity SynonymDB)
keyedSynonymIso = keyedIso synonymIso SynonymDBKey unSynonymDBKey

synonymIso :: Iso' T.Synonym SynonymDB
synonymIso = iso toDb toApi
  where
    toDb = SynonymDB
      <$> view T.synonymKeyword
      <*> view T.synonymSynonyms
      <*> view T.synonymUpdated
    toApi = T.Synonym
      <$> view synonymDBKeyword
      <*> view synonymDBSynonyms
      <*> view synonymDBUpdated
