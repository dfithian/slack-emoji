import ClassyPrelude
import Data.CSV.Conduit (defCSVSettings, writeCSVFile)
import Data.CSV.Conduit.Conversion (Named(Named))
import System.IO (IOMode(WriteMode))
import Text.HTML.Scalpel (Scraper, attr, chroots, hasClass, innerHTML, scrapeURL, text, (//), (@:))
import qualified Types as T

scrapeEntryPage :: String -> String -> IO T.Entry
scrapeEntryPage url keyword = do
  entries <- scrapeURL ("http://slangit.com/" <> url) $ chroots ("table" @: [hasClass "emoticon"]) $ text ("td" @: [hasClass "emote"])
  forM_ entries $ \ xs -> putStrLn $ pack $ "words: " <> intercalate ", " xs
  pure . T.Entry (pack keyword) . map pack . fromMaybe [] $ entries

scrapeListPage :: String -> IO [T.Entry]
scrapeListPage url = do
  xs <- fromMaybe [] <$> scrapeURL url (chroots ("table" // "tbody" // "tr" // "td" // "a" ) getMetadata)
  forM_ xs $ \ (u, k) -> putStrLn $ "url: " <> pack u <> ", keyword: " <> pack k
  forM xs $ uncurry scrapeEntryPage
    where
      getMetadata :: Scraper String (String, String)
      getMetadata = do
        href <- attr "href" "a"
        keyword <- innerHTML "a"
        pure (href, keyword)

main :: IO ()
main = do
  -- TODO convert everything to lower case
  entries <- scrapeListPage "http://slangit.com/emoticons/kaomoji"
  writeCSVFile defCSVSettings "entries.csv" WriteMode (Named <$> entries)
