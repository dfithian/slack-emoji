{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import ClassyPrelude
import Control.Monad.Logger (runLoggingT)
import Data.Time.Clock (addUTCTime)
import Database.Persist.Sqlite (createSqlitePool)
import Emoji (getEmojiR)
import Foundation
  ( App(App, appConnectionPool, appLog, appNextWordsApiRefresh, appRemainingWordsApiRequests, appSettings)
  , Route(EmojiR), resourcesApp )
import qualified Model as M
import Scheduler (wordsApiScheduler)
import Settings (appMaxDbConnections, appSynonymRefreshInterval, appWordsApi, wordsApiLimit)
import Yesod (defaultMakeLogger, mkYesodDispatch, warp)
import Yesod.Core.Types (loggerPutStr)
import Yesod.Default.Config2 (loadYamlSettings, useEnv)

mkYesodDispatch "App" resourcesApp

makeFoundation :: IO App
makeFoundation = do
  now <- liftIO getCurrentTime

  -- load the settings
  appSettings <- loadYamlSettings ["settings.yml"] [] useEnv

  appLog <- (\ logger _ _ _ str -> loggerPutStr logger str) <$> defaultMakeLogger

  -- create the database pool
  appConnectionPool <- flip runLoggingT appLog $
    createSqlitePool "slack-emoji-db" (appMaxDbConnections appSettings)

  appRemainingWordsApiRequests <- newTVarIO . wordsApiLimit . appWordsApi $ appSettings
  appNextWordsApiRefresh <- newTVarIO . flip addUTCTime now . appSynonymRefreshInterval $ appSettings

  let appl = App {..}

  void . fork $ runLoggingT (runReaderT wordsApiScheduler appl) appLog

  pure $ App {..}

startApplication :: IO ()
startApplication = do
  appl <- makeFoundation

  runLoggingT (runReaderT M.seedDb appl) (appLog appl)

  warp 3000 appl
