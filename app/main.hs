{-# OPTIONS_GHC -fno-warn-orphans #-}
import ClassyPrelude
import Control.Monad.Logger (runLoggingT)
import Database.Persist.Sql (runMigration, runSqlPool)
import Database.Persist.Sqlite (createSqlitePool)
import Emoji (getEmojiR, postEmojiR)
import Foundation (App(App, appConnectionPool), Route(EmojiR), resourcesApp, withLogging)
import qualified Model
import Yesod (mkYesodDispatch, warp)

mkYesodDispatch "App" resourcesApp

main :: IO ()
main = do
  -- create the database pool
  appConnectionPool <- flip runLoggingT withLogging $ createSqlitePool (asText "slack-emoji") 1

  -- create tables and wipe them
  flip runSqlPool appConnectionPool $ do
    runMigration Model.migrateAll

  warp 3000 $ App {..}
