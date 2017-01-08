{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import ClassyPrelude
import Control.Monad.Logger (runLoggingT)
import Database.Persist.Sqlite (createSqlitePool)
import Emoji (getEmojiR)
import Foundation
  ( App(App, appConnectionPool, appLogger, appNextWordsApiRefresh, appRemainingWordsApiRequests, appScheduler, appSettings)
  , Route(EmojiR), resourcesApp )
import qualified Model as M
import Network.Wai.Logger (clockDateCacher)
import Scheduler (wordsApiScheduler)
import Settings (appMaxDbConnections, appWordsApi, wordsApiLimit)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)
import Yesod (messageLoggerSource, mkYesodDispatch, warp)
import Yesod.Core.Types (Logger(Logger))
import Yesod.Default.Config2 (loadYamlSettings, useEnv)

mkYesodDispatch "App" resourcesApp

makeFoundation :: IO App
makeFoundation = do
  now <- liftIO getCurrentTime

  -- load the settings
  appSettings <- loadYamlSettings ["settings.yml"] [] useEnv

  -- create the database pool
  let noLogging = \ _ _ _ _ -> pure ()
  appConnectionPool <- flip runLoggingT noLogging $
    createSqlitePool "slack-emoji-db" (appMaxDbConnections appSettings)

  appRemainingWordsApiRequests <- newTVarIO . wordsApiLimit . appWordsApi $ appSettings
  appNextWordsApiRefresh <- newTVarIO now

  appLogger <- do
    logSet <- newStdoutLoggerSet defaultBufSize
    logDate <- fst <$> clockDateCacher
    pure $ Logger logSet logDate

  let appScheduler = error "scheduler not initialized"
      baseAppl = App {..}

  scheduler <- async $ runLoggingT (runReaderT wordsApiScheduler baseAppl) (messageLoggerSource baseAppl appLogger)

  pure $ baseAppl { appScheduler = scheduler }

startApplication :: IO ()
startApplication = do
  appl <- makeFoundation

  runLoggingT (runReaderT M.seedDb appl) (messageLoggerSource appl $ appLogger appl)

  warp 3000 appl
