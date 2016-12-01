module Emoji where

import ClassyPrelude
import Control.Monad.Except (ExceptT, throwError)
import qualified Data.Map as Map
import Network.HTTP.Types (Status, notFound404)
import Web.Scotty (ActionM)

dict :: Map Text Text
dict = Map.fromList [ ("doubleflip", "┻━┻ ︵ ¯\\_(ツ)_/¯ ︵ ┻━┻")
                , ("wide", "( ͡° ͜ʖ ͡°)")
                , ("bear", "ʕ•ᴥ•ʔ")
                , ("afraid", "ಠ_ಠ")
                , ("dance", "(ﾉ◕ヮ◕)ﾉ*:･ﾟ✧ ✧ﾟ･: *ヽ(◕ヮ◕ヽ)")
                , ("orly", "﴾͡๏̯͡๏﴿ O'RLY?")
                , ("cry", "(ಥ﹏ಥ)")
                , ("flip_mad", "(ノಠ益ಠ)ノ彡┻━┻")
                , ("flip_happy", "(╯°□°)╯︵  ┻━┻")
                , ("unimpressed", "ರ_ರ")
                , ("wtf", "¯\\(°_o)/¯")
                , ("reset_table", "┬──┬◡ﾉ(° -°ﾉ)") ]

getValue :: Text -> ExceptT (Status, Text) ActionM Text
getValue key = case (Map.lookup key dict, key) of
    (Just value, _) -> pure value
    (_, "help") -> pure $ "keys: " ++ (concat . intersperse ", " . Map.keys $ dict)
    (Nothing, _) -> throwError (notFound404, "couldn't find key " <> key)
