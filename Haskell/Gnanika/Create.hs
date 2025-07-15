{-# LANGUAGE OverloadedStrings #-}

module Create (createGame) where

import Network.Wreq
import qualified Network.Wreq.Session as Sess
import Control.Lens hiding ((.=))
import Data.Aeson (encode, object, (.=))
import Data.Aeson.Lens (key, _String)
import qualified Data.Text as T
import ApiConfig

createUrl :: String
createUrl = baseUrl ++ "create"

createGame :: Sess.Session -> String -> IO String
createGame sess playerId = do
    let url = createUrl
        payload = encode $ object ["id" .= playerId, "overwrite" .= True]
    r <- Sess.postWith opts sess url payload
    let message = r ^? responseBody . key "message" . _String
        err     = r ^? responseBody . key "error" . _String
    return $ maybe (maybe "Unknown response." T.unpack err) T.unpack message