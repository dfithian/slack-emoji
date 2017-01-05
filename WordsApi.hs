module WordsApi where

import ClassyPrelude
import Control.Lens (_Left, _Nothing, has, over, preview, set, view)
import Control.Lens.TH (makeLenses)
import Control.Monad.Logger (MonadLogger, logWarn)
import Data.Aeson (FromJSON(parseJSON), eitherDecodeStrict', withObject, (.:))
import Foundation (App, appRemainingWordsApiRequests, appSettings)
import Network.HTTP.Types (HeaderName, statusIsSuccessful)
import Network.URI (URI(uriPath))
import Network.Wreq (Response, defaults, header, getWith, responseBody, responseHeader, responseStatus)
import Settings (appWordsApi, wordsApiToken, wordsApiUri)

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

parseWordsApiResponse :: FromJSON a => Response ByteString -> Either Text a
parseWordsApiResponse response =
  case (statusIsSuccessful $ view responseStatus response) of
    True -> over _Left (pack . (<>) "failed to decode response: ") . eitherDecodeStrict' . view responseBody $ response
    False -> either (Left . pack . (<>) "failed to decode response: ") (Left . (<>) "request failed: " . view wordsApiErrorMessage)
      . eitherDecodeStrict' . view responseBody $ response

wordsApiRequest :: (MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadReader App m, FromJSON a) => Text -> m (Either Text a)
wordsApiRequest path = do
  remainingRequestsTv <- asks appRemainingWordsApiRequests
  remainingRequests <- liftIO . atomically . readTVar $ remainingRequestsTv
  case (remainingRequests > 0) of
    False -> pure $ Left "no requests remaining"
    True -> do
      uri <- asks (wordsApiUri . appWordsApi . appSettings)
      token <- asks (wordsApiToken . appWordsApi . appSettings)
      let opts = set (header requestMashapeKey) [encodeUtf8 token] defaults
      rawResponse <- liftIO $ getWith opts (show $ uri { uriPath = unpack path })
      let requestsRemainingMay = do
            hdr <- preview (responseHeader requestsRemainingHeader) rawResponse
            readMay $ decodeUtf8 hdr
      when (has _Nothing requestsRemainingMay) $ $logWarn "Unable to determine how many rate-limited requests remaining"
      liftIO . atomically . writeTVar remainingRequestsTv . fromMaybe (remainingRequests - 1) $ requestsRemainingMay
      pure . parseWordsApiResponse . over responseBody toStrict $ rawResponse
