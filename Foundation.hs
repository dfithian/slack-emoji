module Foundation (module Foundation, Route(..)) where

import ClassyPrelude
import Control.Lens (_2, over)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr)
import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Random (MonadRandom, RVar, StdRandom(StdRandom), runRVar)
import Database.Persist.Sql (ConnectionPool, SqlPersistT, runSqlPool)
import Network.HTTP.Types (Status, badRequest400, internalServerError500, notFound404, ok200)
import Yesod.Core (MonadHandler, Route, Yesod, renderRoute, sendStatusJSON)
import Yesod.Core.Dispatch (mkYesodData, parseRoutes)

data App = App
  { appConnectionPool :: ConnectionPool
  }

mkYesodData "App" [parseRoutes|
/ EmojiR GET POST
|]

instance Yesod App

withLogging :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
withLogging _ _ _ _ = pure ()

runDb :: (MonadBaseControl IO m, MonadReader App m) => SqlPersistT m a -> m a
runDb action = do
  pool <- asks appConnectionPool
  runSqlPool action pool

runRandom :: (MonadIO m, MonadRandom m) => RVar a -> m a
runRandom = flip runRVar StdRandom

type ApiResult m a = ExceptT (Status, Text) m a

newtype ApiErrorReason = ApiErrorReason { unApiErrorReason :: Text }
instance ToJSON ApiErrorReason where
  toJSON (ApiErrorReason reason) = object ["message" .= reason]

apiBadRequest :: MonadBaseControl IO m => Text -> ApiResult m a
apiBadRequest = throwError . (badRequest400,)

apiNotFound :: MonadBaseControl IO m => Text -> ApiResult m a
apiNotFound = throwError . (notFound404,)

apiInternalError :: MonadBaseControl IO m => Text -> ApiResult m a
apiInternalError = throwError . (internalServerError500,)

runApiResult :: (MonadBaseControl IO m, MonadHandler m) => forall a . ToJSON a => ApiResult m a -> m ()
runApiResult result = either (uncurry sendStatusJSON . over _2 ApiErrorReason) (sendStatusJSON ok200) =<< runExceptT result
