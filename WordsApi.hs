module WordsApi where

import ClassyPrelude
import Control.Lens (_Left, _Nothing, has, over, preview, set, view)
import Control.Lens.TH (makeLenses)
import Control.Monad.Logger (MonadLogger, logWarn)
import Data.Aeson (FromJSON(parseJSON), eitherDecodeStrict', withObject, (.:))
import Foundation (App, appRemainingWordsApiRequests, appSettings)
import Network.HTTP.Types (HeaderName, statusIsSuccessful)
import Network.URI (URI(uriPath))
import Network.Wreq (Response, checkResponse, defaults, header, getWith, responseBody, responseHeader, responseStatus)
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
  let body = view responseBody response
  in case view responseStatus response of
    (statusIsSuccessful -> True) ->
      over _Left (\ msg -> pack $ "failed to decode response \'" <> unpack (decodeUtf8 body) <> "\': " <> msg) $ eitherDecodeStrict' body
    status -> Left $ "failed with error " <> tshow status

wordsApiRequest :: (MonadIO m, MonadLogger m, MonadReader App m, FromJSON a) => Text -> m (Either Text a)
wordsApiRequest path = do
  remainingRequestsTv <- view appRemainingWordsApiRequests
  remainingRequests <- liftIO . atomically . readTVar $ remainingRequestsTv
  case (remainingRequests > 0) of
    False -> pure $ Left "no requests remaining"
    True -> do
      uri <- wordsApiUri . appWordsApi <$> view appSettings
      token <- wordsApiToken . appWordsApi <$> view appSettings
      let opts = set (header requestMashapeKey) [encodeUtf8 token]
                 . set checkResponse Nothing
                 $ defaults
      rawResponse <- liftIO $ getWith opts (show $ uri { uriPath = unpack path })
      let requestsRemainingMay = do
            hdr <- preview (responseHeader requestsRemainingHeader) rawResponse
            readMay $ decodeUtf8 hdr
      when (has _Nothing requestsRemainingMay) $ $logWarn "Unable to determine how many rate-limited requests remaining"
      liftIO . atomically . writeTVar remainingRequestsTv . fromMaybe (remainingRequests - 1) $ requestsRemainingMay
      pure . parseWordsApiResponse . over responseBody toStrict $ rawResponse
