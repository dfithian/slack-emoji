{-# OPTIONS_GHC -fno-warn-orphans #-}
import ClassyPrelude
import Control.Monad.Logger (runLoggingT)
import Database.Persist.Sql (runMigration, runSqlPool)
import Database.Persist.Sqlite (createSqlitePool)
import Emoji (getEmojiR, postEmojiR)
import Foundation (App(App, appConnectionPool, appRemainingWordsApiRequests, appSettings), Route(EmojiR), resourcesApp, withLogging)
import qualified Model
import Settings (appWordsApi, wordsApiLimit)
import Yesod (mkYesodDispatch, warp)
import Yesod.Default.Config2 (loadYamlSettings, useEnv)

mkYesodDispatch "App" resourcesApp

main :: IO ()
main = do
  -- load the settings
  appSettings <- loadYamlSettings ["settings.yml"] [] useEnv

  -- create the database pool
  appConnectionPool <- flip runLoggingT withLogging $ createSqlitePool (asText "slack-emoji-db") 1

  -- create tables and wipe them
  flip runSqlPool appConnectionPool $ do
    runMigration Model.migrateAll

  appRemainingWordsApiRequests <- newTVarIO . wordsApiLimit . appWordsApi $ appSettings

  warp 3000 $ App {..}
