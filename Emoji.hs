module Emoji where

import ClassyPrelude hiding (Handler)
import Control.Lens (_Just, each, from, toListOf, preview, view)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NEL
import Data.Random.Extras (choice)
import Database.Persist (SelectOpt(LimitTo), selectFirst, selectList, (==.))
import Foundation (ApiResult, App, Handler, apiBadRequest, apiNotFound, asTypedContent, runApiResult, runDb, runRandom)
import qualified Model as M
import qualified Types as T
import Yesod.Core (lookupGetParam)

oneOf :: NonEmpty a -> IO a
oneOf (x :| []) = pure x
oneOf (x :| xs) = runRandom $ choice (x:xs)

recurseEntryForKeyword :: (MonadBaseControl IO m, MonadIO m, MonadReader App m) => Text -> ApiResult m (Maybe Text)
recurseEntryForKeyword keyword = do
  let entryForKeyword k = do
        entries <- fromMaybe [] . preview (_Just . from M.keyedEntryIso . T.ent . T.entryEntries) <$> runDb (selectFirst [M.EntryDBKeyword ==. k] [])
        liftIO . traverse oneOf $ NEL.nonEmpty entries
  keywordEntry <- entryForKeyword keyword
  -- fold over the keyword and list of synonyms and use applicative + monad to fetch the next possible emoji
  foldM (\ previous next -> maybe (entryForKeyword next) (pure . Just) previous) keywordEntry
    =<< view (_Just . from M.keyedSynonymIso . T.ent . T.synonymSynonyms)
    <$> runDb (selectFirst [M.SynonymDBKeyword ==. keyword] []) -- get the list of synonyms

-- |Look up an emoji by keyword or using an alternative synonym
getEmojiR :: Handler ()
getEmojiR = do
  serialize <- asTypedContent True
  runApiResult serialize $ do
    keyword <- maybe (apiBadRequest "missing param \'text\'") (pure . toLower) =<< lift (lookupGetParam "text")
    entryMay <- recurseEntryForKeyword keyword -- look up an emoji for the keyword or try a synonym
    case entryMay of
      Nothing -> do
        allKeys <- toListOf (each . from M.keyedEntryIso . T.ent . T.entryKeyword) <$> runDb (selectList [] [LimitTo 100])
        case NEL.nonEmpty allKeys of
          Just ks -> do
            suggestion <- liftIO $ oneOf ks
            apiNotFound $ "No entries for \'" <> keyword <> "\'; try \'" <> suggestion <> "\' instead"
          Nothing -> apiNotFound $ "No entries for \'" <> keyword <> "\'"
      Just x -> pure x
