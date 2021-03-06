import ClassyPrelude
import Conduit (ConduitT, mapC, runResourceT, sinkFile)
import Data.Conduit (connect, fuse)
import Data.Conduit.List (sourceList)
import Data.CSV.Conduit (defCSVSettings, fromCSV, writeHeaders)
import Data.CSV.Conduit.Conversion (toNamedRecord)
import Data.List (nub)
import Text.HTML.Scalpel
  ( Config(Config), Scraper
  , attr, chroots, hasClass, innerHTML, scrapeURL, scrapeURLWithConfig, text, utf8Decoder
  , (//), (@:) )
import qualified Types as T

defaultConfig :: Config Text
defaultConfig = Config utf8Decoder Nothing

scrapeSlangitEntryPage :: Text -> Text -> IO T.Entry
scrapeSlangitEntryPage url keyword = do
  entries <- scrapeURLWithConfig defaultConfig ("http://slangit.com/" <> unpack url) $
    chroots ("table" @: [hasClass "emoticon"]) $ text ("td" @: [hasClass "emote"])
  forM_ entries $ \ xs -> putStrLn $ "words: " <> intercalate ", " xs
  pure . T.Entry keyword . fromMaybe [] $ entries

scrapeSlangitListPage :: String -> IO [T.Entry]
scrapeSlangitListPage url = do
  xs <- fromMaybe [] <$> scrapeURL url (chroots ("table" // "tbody" // "tr" // "td" // "a") getMetadata)
  forM_ xs $ \ (u, ks) -> putStrLn $ "url: " <> u <> ", keywords: " <> intercalate ", " ks
  map join $ forM xs $ \ (u, ks) -> forM ks $ scrapeSlangitEntryPage u
    where
      getMetadata :: Scraper Text (Text, [Text])
      getMetadata = do
        href <- attr "href" "a"
        keyword <- toLower <$> innerHTML "a"
        let keywords = nub $ keyword:(words keyword)
        pure (href, keywords)

main :: IO ()
main = do
  slangitEntries <- scrapeSlangitListPage "http://slangit.com/emoticons/kaomoji"
  let headerRow :: forall m . Monad m => ConduitT (Map ByteString ByteString) ByteString m ()
      headerRow = writeHeaders defCSVSettings
  runResourceT $ do
    sourceList slangitEntries
      `fuse` mapC toNamedRecord
      `fuse` (headerRow >> fromCSV defCSVSettings)
      `connect` sinkFile "entries.csv"
