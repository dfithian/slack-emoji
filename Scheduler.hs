module Scheduler where

import ClassyPrelude
import Conduit (mapM_C)
import Control.Lens (_Right, each, from, over, to, toListOf, view)
import Control.Monad.Logger (MonadLogger, logError)
import Data.Conduit (connect, yieldM)
import Data.Time.Clock (addUTCTime)
import Database.Persist (SelectOpt(LimitTo), repsert, selectList, (<.))
import Foundation (App, appSettings, appNextWordsApiRefresh, runDb)
import qualified Model as M
import Settings (appSynonymRefreshInterval)
import qualified Types as T
import WordsApi (wordsApiRequest, wordsApiSynonymsSynonyms)

-- |Schedule calls to the Words API
--
-- Every ten seconds, look up 'stale' synonyms (limit 10) and refresh them via the Words API
wordsApiScheduler :: (MonadBaseControl IO m, MonadCatch m, MonadIO m, MonadLogger m, MonadReader App m) => m ()
wordsApiScheduler = do
  refreshInterval <- asks $ appSynonymRefreshInterval . appSettings
  nextRefreshTv <- asks appNextWordsApiRefresh
  let scheduler = forever $ liftIO (threadDelay 10000000) >> pure () -- thunk!
      monitor _ = do
        now <- liftIO getCurrentTime
        nextRefresh <- liftIO . atomically . readTVar $ nextRefreshTv
        when (now > nextRefresh) $ do
          outdated <- toListOf (each . from M.keyedSynonymIso)
            <$> runDb (selectList [M.SynonymDBUpdated <. addUTCTime (negate refreshInterval) now] [LimitTo 10])
          mapM_ updateFor outdated
          liftIO . atomically . writeTVar nextRefreshTv . addUTCTime refreshInterval $ now
  catch (yieldM scheduler `connect` mapM_C monitor) -- source thunks and sink effects
         (\ (se :: SomeException) -> $logError $ "Scheduler failed with " <> tshow se)
    where
      updateFor synonym = do
        let keyword = view (T.ent . T.synonymKeyword) synonym
        now <- liftIO getCurrentTime
        newSynonymsEither <- over _Right (view wordsApiSynonymsSynonyms) <$> wordsApiRequest ("/words/" <> keyword <> "/synonyms")
        case newSynonymsEither of
          Left errorMsg -> $logError $ "Failed to retrieve new synonyms for keyword " <> keyword <> " due to " <> errorMsg
          Right newSynonyms ->
            let dbKey = view (T.key . to M.SynonymDBKey) synonym
                ent = view M.synonymIso $ T.Synonym keyword newSynonyms now
            in runDb $ repsert dbKey ent
