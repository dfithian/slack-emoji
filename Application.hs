{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import ClassyPrelude
import Control.Monad.Logger (runLoggingT)
import Database.Persist.Sqlite (createSqlitePool)
import Emoji (getEmojiR)
import Foundation
  ( App (App, _appConnectionPool, _appLogger, _appNextWordsApiRefresh, _appRemainingWordsApiRequests, _appSettings)
  , Route (EmojiR), resourcesApp )
import qualified Model as M
import Network.Wai.Logger (clockDateCacher)
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
  _appSettings <- loadYamlSettings ["settings.yml"] [] useEnv

  -- create the database pool
  let noLogging = \ _ _ _ _ -> pure ()
  _appConnectionPool <- flip runLoggingT noLogging $
    createSqlitePool "slack-emoji-db" (appMaxDbConnections _appSettings)

  _appRemainingWordsApiRequests <- newTVarIO . wordsApiLimit . appWordsApi $ _appSettings
  _appNextWordsApiRefresh <- newTVarIO now

  _appLogger <- do
    logSet <- newStdoutLoggerSet defaultBufSize
    logDate <- fst <$> clockDateCacher
    pure $ Logger logSet logDate

  pure App {..}

startApplication :: IO ()
startApplication = do
  appl <- makeFoundation

  runLoggingT (runReaderT M.seedDb appl) (messageLoggerSource appl $ _appLogger appl)

  warp 3000 appl
