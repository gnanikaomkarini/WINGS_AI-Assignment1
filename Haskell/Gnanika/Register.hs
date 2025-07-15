{-# LANGUAGE OverloadedStrings #-}

module Register (register) where

import Network.Wreq
import qualified Network.Wreq.Session as Sess
import Control.Lens hiding ((.=))
import Data.Aeson (encode, object, (.=))
import Data.Aeson.Lens (key, _String)
import qualified Data.Text as T
import ApiConfig

registerUrl :: String
registerUrl = baseUrl ++ "register"

gameMode :: String
gameMode = "wordle"

register :: Sess.Session -> String -> IO String
register sess name = do
    let url = registerUrl
        payload = encode $ object ["mode" .= gameMode, "name" .= name]
    r <- Sess.postWith opts sess url payload
    let playerId = r ^? responseBody . key "id" . _String
    return $ maybe "" T.unpack playerId
