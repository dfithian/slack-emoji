module Emoji where

import ClassyPrelude hiding (Handler)
import Conduit (mapM_C)
import Control.Lens (_Just, each, from, over, toListOf, preview, view)
import Control.Monad.Logger (MonadLogger)
import Data.Conduit (connect, fuse)
import Data.CSV.Conduit (defCSVSettings, intoCSV)
import Data.CSV.Conduit.Conversion (Named(Named))
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NEL
import Data.Random.Extras (choice)
import Database.Persist (SelectOpt(LimitTo), entityKey, entityVal, insert_, repsert, selectFirst, selectList, (==.))
import Foundation (ApiResult, App, Handler, apiBadRequest, apiNotFound, asTypedContent, runApiResult, runDb, runRandom)
import qualified Model as M
import qualified Types as T
import WordsApi (getOrUpdateSynonymsForKeyword)
import Yesod.Core (fileSource, lookupFile, lookupGetParam, toTypedContent)

oneOf :: NonEmpty a -> IO a
oneOf (x :| []) = pure x
oneOf (x :| xs) = runRandom $ choice (x:xs)

entryForKeyword :: (MonadBaseControl IO m, MonadIO m, MonadReader App m) => Text -> ApiResult m (Maybe Text)
entryForKeyword keyword = do
  entries <- fromMaybe [] . preview (_Just . from M.keyedEntryIso . T.ent . T.entryEntries) <$> runDb (selectFirst [M.EntryDBKeyword ==. keyword] [])
  liftIO . traverse oneOf $ NEL.nonEmpty entries

recurseEntryForKeyword :: (MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadReader App m) => Text -> ApiResult m (Maybe Text)
recurseEntryForKeyword keyword = do
  synonyms <- getOrUpdateSynonymsForKeyword keyword
  headMay . catMaybes <$> traverse entryForKeyword (keyword:synonyms)

getEmojiR :: Handler ()
getEmojiR = do
  serialize <- asTypedContent True
  runApiResult serialize $ do
    keyword <- maybe (apiBadRequest "missing param \'text\'") (pure . toLower) =<< lift (lookupGetParam "text")
    entryMay <- recurseEntryForKeyword keyword
    case entryMay of
      Nothing -> do
        allKeys <- toListOf (each . from M.keyedEntryIso . T.ent . T.entryKeyword) <$> runDb (selectList [] [LimitTo 100])
        case NEL.nonEmpty allKeys of
          Just ks -> do
            suggestion <- liftIO $ oneOf ks
            apiNotFound $ "No entries for \'" <> keyword <> "\'; try \'" <> suggestion <> "\' instead"
          Nothing -> apiNotFound $ "No entries for \'" <> keyword <> "\'"
      Just x -> pure x

-- handle a csv upload
postEmojiR :: Handler ()
postEmojiR = runApiResult (const $ toTypedContent ()) $ do
  fileInfo <- maybe (apiBadRequest "No file provided") pure =<< lookupFile "file"
  fileSource fileInfo
    `fuse` intoCSV defCSVSettings
    `connect` mapM_C ( \ (Named new) -> do
                         prevMay <- preview (_Just . from M.keyedEntryIso) <$> runDb (selectFirst [M.EntryDBKeyword ==. view T.entryKeyword new] [])
                         case prevMay of
                           Just prev -> -- merge existing entries
                             let replacement = over (T.ent . T.entryEntries) (nub . (<> view T.entryEntries new)) prev
                                 (replacementKey, replacementEntity) = entityKey &&& entityVal $ view M.keyedEntryIso replacement
                             in runDb $ repsert replacementKey replacementEntity
                           Nothing -> runDb $ insert_ $ view M.entryIso new
                     )
