module Foundation (module Foundation, Route(..)) where

import ClassyPrelude hiding (intersect, span)
import Control.Lens (_2, lens, makeClassy, over, view)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Random (MonadRandom, RVar, StdRandom(StdRandom), runRVar)
import Data.List (intersect)
import Data.Text (split)
import Database.Persist.Sql (ConnectionPool, SqlPersistT, runSqlPool)
import Network.HTTP.Types (Status, hAccept, badRequest400, internalServerError500, notFound404, ok200)
import Settings (AppSettings)
import Text.Blaze.Html5 (body, span)
import Yesod.Core
  ( MonadHandler, Route, TypedContent(TypedContent), Yesod(makeLogger)
  , renderRoute, sendResponseStatus
  , lookupHeader, toContent, toHtml, typeHtml, typeJson )
import Yesod.Core.Dispatch (mkYesodData, parseRoutes)
import Yesod.Core.Types (HandlerData, Logger, handlerEnv, rheSite)

data App = App
  { _appConnectionPool            :: ConnectionPool
  -- ^ The database connection pool
  , _appSettings                  :: AppSettings
  -- ^ Settings as defined in "Settings"
  , _appRemainingWordsApiRequests :: TVar Int
  -- ^ Remaining requests to the rate-limited words api
  , _appNextWordsApiRefresh       :: TVar UTCTime
  -- ^ The next time to check for synonym refresh
  , _appLogger                    :: Logger
  -- ^ The logger for the application, the very same that Yesod uses
  }

makeClassy ''App

mkYesodData "App" [parseRoutes|
/ EmojiR GET
|]

instance Yesod App where
  makeLogger = pure . view appLogger

instance HasApp (HandlerData child App) where
  app = lens get set
    where
      get = rheSite . handlerEnv
      set d x =
        let e = handlerEnv d
        in d { handlerEnv = e { rheSite = x } }

runDb :: (HasApp r, MonadIO m, MonadReader r m) => SqlPersistT IO a -> m a
runDb action = do
  pool <- view appConnectionPool
  liftIO $ runSqlPool action pool

runRandom :: MonadRandom m => RVar a -> m a
runRandom = flip runRVar StdRandom

type ApiResult m a = ExceptT (Status, Text) m a

newtype ApiErrorReason = ApiErrorReason { unApiErrorReason :: Text }
instance ToJSON ApiErrorReason where
  toJSON (ApiErrorReason reason) = object ["message" .= reason]

apiBadRequest :: MonadIO m => Text -> ApiResult m a
apiBadRequest = throwError . (badRequest400,)

apiNotFound :: MonadIO m => Text -> ApiResult m a
apiNotFound = throwError . (notFound404,)

apiInternalError :: MonadIO m => Text -> ApiResult m a
apiInternalError = throwError . (internalServerError500,)

asHtml :: Text -> TypedContent
asHtml v = TypedContent typeHtml . toContent . body . span . toHtml $ v

asJson :: Bool -> Text -> TypedContent
asJson b v = TypedContent typeJson . toContent . object . catMaybes $
  [ if b then Just ("response_type" .= asText "in_channel") else Nothing
  , Just ("text" .= v) ]

asTypedContent :: MonadHandler m => Bool -> m (Text -> TypedContent)
asTypedContent b = do
  accept <- split (flip elem [',', ';']) . decodeUtf8 . fromMaybe "application/json" <$> lookupHeader hAccept
  let validAcceptTypes = ["text/html", "application/json"]
  pure $ case headMay (intersect accept validAcceptTypes) of
    Just "text/html" -> asHtml
    Just "application/json" -> asJson b
    _ -> asJson b

runApiResult :: (MonadIO m, MonadHandler m) => (a -> TypedContent) -> ApiResult m a -> m ()
runApiResult serialize result = do
  onError <- asTypedContent False
  (status, message) <- either (over _2 onError) ((ok200,) . serialize) <$> runExceptT result
  sendResponseStatus status message
