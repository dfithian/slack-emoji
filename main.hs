{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod
import           Prelude hiding (lookup)
import           Data.Text (Text, pack)
import           Data.Map (lookup, fromList, keys)
import           Data.List (intersperse)

data SlackEmoji = SlackEmoji

dict = fromList [("doubleflip", "┻━┻ ︵ ¯\\_(ツ)_/¯ ︵ ┻━┻")]

getKey :: MonadHandler m => Maybe Text -> m (Text)
getKey maybeKey = case maybeKey of
    Just key -> return key
    Nothing -> invalidArgs ["failed to find argument"]

getValue :: MonadHandler m => Text -> m (Text)
getValue key = case (lookup key dict, key) of
    (Just value, _) -> return value
    (_, "help") -> return $ pack $ "keys: " ++ (concat $ intersperse ", " $ keys dict)
    (Nothing, _) -> notFound

mkYesod "SlackEmoji" [parseRoutes|
/ Emoji GET
|]

instance Yesod SlackEmoji

getEmoji :: Handler TypedContent
getEmoji = do
    maybeKey <- lookupGetParam "text"
    key <- getKey maybeKey
    value <- getValue key
    selectRep $ do
        provideRep $ return $ object
            [ "response_type" .= ("in_channel" :: Text)
            , "text" .= value ]

main :: IO ()
main = warp 3000 SlackEmoji

