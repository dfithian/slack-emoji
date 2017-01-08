module Scheduler where

import ClassyPrelude
import Control.Lens (_Right, each, from, over, to, toListOf, view)
import Control.Monad.Logger (MonadLogger, logError, logInfo)
import Data.Time.Clock (addUTCTime)
import Database.Persist (SelectOpt(LimitTo), repsert, selectList, (<.))
import Foundation (App, appSettings, appNextWordsApiRefresh, runDb)
import qualified Model as M
import Settings (appSynonymRefreshInterval, appSynonymValidityTime)
import qualified Types as T
import WordsApi (wordsApiRequest, wordsApiSynonymsSynonyms)

-- |Schedule calls to the Words API
--
-- Every ten seconds, look up 'stale' synonyms (limit 10) and refresh them via the Words API
wordsApiScheduler :: (MonadBaseControl IO m, MonadCatch m, MonadIO m, MonadLogger m, MonadReader App m) => m ()
wordsApiScheduler = do
  refreshInterval <- asks $ appSynonymRefreshInterval . appSettings
  validityTime <- asks $ appSynonymValidityTime . appSettings
  nextRefreshTv <- asks appNextWordsApiRefresh
  $logInfo "Starting words API scheduler"

  let scheduler = do
        now <- liftIO getCurrentTime
        nextRefresh <- liftIO . atomically . readTVar $ nextRefreshTv
        when (now > nextRefresh) $ do
          $logInfo "Updating stale synonyms"
          outdated <- toListOf (each . from M.keyedSynonymIso)
            <$> runDb (selectList [M.SynonymDBUpdated <. addUTCTime (negate validityTime) now] [LimitTo 1000])
          mapM_ updateFor outdated
          liftIO . atomically . writeTVar nextRefreshTv . addUTCTime refreshInterval $ now

  forever $ do
    liftIO (threadDelay 10000000)
    catch scheduler (\ (se :: SomeException) -> $logError $ "Scheduler failed with " <> tshow se)

    where
      updateFor synonym = do
        let keyword = view (T.ent . T.synonymKeyword) synonym
            dbKey = view (T.key . to M.SynonymDBKey) synonym
        now <- liftIO getCurrentTime
        newSynonymsEither <- over _Right (view wordsApiSynonymsSynonyms) <$> wordsApiRequest ("/words/" <> keyword <> "/synonyms")
        case newSynonymsEither of
          Left errorMsg -> do
            $logError $ "Failed to retrieve new synonyms for keyword \'" <> keyword <> "\' due to " <> errorMsg
            runDb . repsert dbKey . view M.synonymIso $ T.Synonym keyword [] now
          Right newSynonyms -> do
            $logInfo $ "Updating synonyms for keyword \'" <> keyword <> "\'"
            runDb . repsert dbKey . view M.synonymIso $ T.Synonym keyword newSynonyms now
