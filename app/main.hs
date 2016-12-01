import ClassyPrelude
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr, runLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (ToJSON, object, (.=))
import Database.Persist.Sql (ConnectionPool, runMigration)
import Database.Persist.Sqlite (SqlPersistT, createSqlitePool, runSqlPool)
import Emoji (getValue)
import qualified Model
import Network.HTTP.Types (Status)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty (ActionM, get, json, middleware, param, scotty, status, text)

main :: IO ()
main = do
  -- create the database pool
  pool <- flip runLoggingT noLogging $ createSqlitePool dbName 1
  -- create tables and wipe them
  flip runSqlPool pool $ do
    runMigration Model.migrateAll

  -- start a server on port 3000
  scotty 3000 $ do
    middleware logStdoutDev

    get "/api/emoji" $ runApiResult $ do
      k <- lift $ param "text"
      v <- getValue k
      pure $ object
        [ "response_type" .= asText "in_channel"
        , "text" .= v ]

  where
    dbName :: Text
    dbName = "slack-emoji"

    noLogging :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    noLogging _ _ _ _ = pure ()

    runDb :: MonadBaseControl IO m => ConnectionPool -> SqlPersistT m a -> m a
    runDb pool = flip runSqlPool pool

    runApiResult :: forall a . ToJSON a => ExceptT (Status, Text) ActionM a -> ActionM ()
    runApiResult result = either (\ (code, msg) -> text (fromStrict msg) >> status code) json =<< runExceptT result
