module Emoji where

import ClassyPrelude hiding (Handler)
import Control.Lens (_Just, each, from, to, toListOf, preview, view)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NEL
import Data.Random.Extras (choice)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Persist (SelectOpt(LimitTo), insert_, selectFirst, selectList, (==.))
import Foundation (ApiResult, App, Handler, apiBadRequest, apiNotFound, asTypedContent, runApiResult, runDb, runRandom)
import qualified Model as M
import qualified Types as T
import Yesod.Core (lookupGetParam)

oneOf :: NonEmpty a -> IO a
oneOf (x :| []) = pure x
oneOf (x :| xs) = runRandom $ choice (x:xs)

entryForKeyword :: (MonadBaseControl IO m, MonadIO m, MonadReader App m) => Text -> ApiResult m (Maybe T.Entry)
entryForKeyword k = preview (_Just . from M.keyedEntryIso . T.ent) <$> runDb (selectFirst [M.EntryDBKeyword ==. k] [])

-- |Look up an emoji by keyword or using an alternative synonym
getEmojiR :: Handler ()
getEmojiR = do
  serialize <- asTypedContent True
  runApiResult serialize $ do
    keyword <- maybe (apiBadRequest "missing param \'text\'") (pure . toLower) =<< lift (lookupGetParam "text")
    keywordEntryMay <- entryForKeyword keyword
    keywordSynonymsMay <- preview (_Just . from M.keyedSynonymIso) <$> runDb (selectFirst [M.SynonymDBKeyword ==. keyword] [])
    resultMay <- case (keywordEntryMay, keywordSynonymsMay) of
      (Just (view (T.entryEntries . to NEL.nonEmpty) -> Just entries), _) -> liftIO . map Just . oneOf $ entries
      (_, Just (view (T.ent . T.synonymSynonyms . to NEL.nonEmpty) -> Just synonyms)) ->
        let getEntry k = liftIO . traverse oneOf . NEL.nonEmpty . fromMaybe [] . preview (_Just . T.entryEntries) =<< entryForKeyword k
        in foldM (\ prev next -> maybe (getEntry next) (pure . Just) prev) Nothing $ NEL.toList synonyms
      (_, Just _) -> pure Nothing
      (_, Nothing) -> do
        runDb . insert_ . view M.synonymIso . T.Synonym keyword [] . posixSecondsToUTCTime . fromInteger $ 0
        pure Nothing
    case resultMay of
      Just result -> pure result
      Nothing -> do
        allKeys <- toListOf (each . from M.keyedEntryIso . T.ent . T.entryKeyword) <$> runDb (selectList [] [LimitTo 100])
        case NEL.nonEmpty allKeys of
          Just ks -> do
            suggestion <- liftIO $ oneOf ks
            apiNotFound $ "No entries for \'" <> keyword <> "\'; try \'" <> suggestion <> "\' instead"
          Nothing -> apiNotFound $ "No entries for \'" <> keyword <> "\'"
