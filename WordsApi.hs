module WordsApi where

import ClassyPrelude
import Control.Lens (_Just, _Nothing, from, has, over, preview, set, view)
import Control.Lens.TH (makeLenses)
import Control.Monad.Logger (MonadLogger, logWarn)
import Data.Aeson (FromJSON(parseJSON), eitherDecodeStrict', withObject, (.:))
import Data.Time.Clock (addUTCTime)
import Database.Persist (insert_, repsert, selectFirst, (==.))
import Foundation (ApiResult, App, apiInternalError, appRemainingWordsApiRequests, appSettings, runDb)
import qualified Model as M
import Network.HTTP.Types (HeaderName, statusIsSuccessful)
import Network.URI (URI(uriPath))
import Network.Wreq (Response, defaults, header, getWith, responseBody, responseHeader, responseStatus)
import Settings (appSynonymValidityTime, appWordsApi, wordsApiToken, wordsApiUri)
import qualified Types as T

requestsRemainingHeader :: HeaderName
requestsRemainingHeader = "X-Ratelimit-Requests-Remaining"

requestMashapeKey :: HeaderName
requestMashapeKey = "X-Mashape-Key"

data WordsApiError = WordsApiError
  { _wordsApiErrorMessage :: Text
  }

data WordsApiSynonyms = WordsApiSynonyms
  { _wordsApiSynonymsWord     :: Text
  , _wordsApiSynonymsSynonyms :: [Text]
  }

makeLenses ''WordsApiError
makeLenses ''WordsApiSynonyms

instance FromJSON WordsApiError where
  parseJSON = withObject "WordsApiError" $ \ obj -> WordsApiError <$> obj .: "message"

instance FromJSON WordsApiSynonyms where
  parseJSON = withObject "WordsApiSynonyms" $ \ obj ->
    WordsApiSynonyms <$> obj .: "word" <*> obj .: "synonyms"

parseWordsApiResponse :: (MonadBaseControl IO m, FromJSON a) => Response ByteString -> ApiResult m a
parseWordsApiResponse response =
  case (statusIsSuccessful $ view responseStatus response) of
    True -> either (apiInternalError . pack) pure . eitherDecodeStrict' . view responseBody $ response
    False -> either (apiInternalError . pack) (apiInternalError . view wordsApiErrorMessage) . eitherDecodeStrict' . view responseBody $ response

wordsApiRequest :: (MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadReader App m, FromJSON a) => Text -> ApiResult m a
wordsApiRequest path = do
  remainingRequestsTv <- asks appRemainingWordsApiRequests
  remainingRequests <- liftIO . atomically . readTVar $ remainingRequestsTv
  uri <- asks (wordsApiUri . appWordsApi . appSettings)
  token <- asks (wordsApiToken . appWordsApi . appSettings)
  let opts = set (header requestMashapeKey) [encodeUtf8 token] defaults
  rawResponse <- liftIO $ getWith opts (show $ uri { uriPath = unpack path })
  let requestsRemainingMay = do
        hdr <- preview (responseHeader requestsRemainingHeader) rawResponse
        readMay $ decodeUtf8 hdr
  when (has _Nothing requestsRemainingMay) $ $logWarn "unable to determine how many rate-limited requests remaining"
  liftIO . atomically . writeTVar remainingRequestsTv . fromMaybe (remainingRequests - 1) $ requestsRemainingMay
  parseWordsApiResponse $ over responseBody toStrict rawResponse

getOrUpdateSynonymsForKeyword :: (MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadReader App m) => Text -> ApiResult m [Text]
getOrUpdateSynonymsForKeyword keyword = do
  now <- liftIO getCurrentTime
  synonymValidityTime <- asks (appSynonymValidityTime . appSettings)
  remainingRequestsTv <- asks appRemainingWordsApiRequests
  remainingRequests <- liftIO . atomically . readTVar $ remainingRequestsTv
  synonymMay <- preview (_Just . from M.keyedSynonymIso) <$> runDb (selectFirst [M.SynonymDBKeyword ==. keyword] [])
  let updateLowerBound = negate synonymValidityTime `addUTCTime` now
  case (synonymMay, remainingRequests) of
    (Just synonym@((>= updateLowerBound) . view (T.ent . T.synonymUpdated) -> True), _) -> pure $ view (T.ent . T.synonymSynonyms) synonym
    (_, 0) -> pure []
    _ -> do
      newSynonyms <- view wordsApiSynonymsSynonyms <$> wordsApiRequest ("/words/" <> keyword <> "/synonyms")
      let keyMay = M.SynonymDBKey <$> preview (_Just . T.key) synonymMay
          ent = view M.synonymIso $ T.Synonym keyword newSynonyms now
      runDb $ maybe (insert_ ent) (flip repsert ent) keyMay
      pure newSynonyms
