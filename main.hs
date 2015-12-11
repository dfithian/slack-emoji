{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod
import           Prelude hiding (lookup)
import           Data.Text (Text)
import           Data.Map (lookup, fromList)

data SlackEmoji = SlackEmoji

dict = fromList [("doubleflip", "┻━┻ ︵ ¯\\_(ツ)_/¯ ︵ ┻━┻")]

getKey :: MonadHandler m => Maybe Text -> m (Text)
getKey maybeKey = case maybeKey of
    Just key -> return key
    Nothing -> invalidArgs ["missing argument id"]

getValue :: MonadHandler m => Text -> m (Text)
getValue key = case lookup key dict of
    Just value -> return value
    Nothing -> notFound

mkYesod "SlackEmoji" [parseRoutes|
/ Emoji GET
|]

instance Yesod SlackEmoji

getEmoji :: Handler TypedContent
getEmoji = do
    maybeKey <- lookupGetParam "id"
    key <- getKey maybeKey
    value <- getValue key
    selectRep $ do
        provideRep $ return $ object
            [ "value" .= value ]

main :: IO ()
main = warp 3000 SlackEmoji

