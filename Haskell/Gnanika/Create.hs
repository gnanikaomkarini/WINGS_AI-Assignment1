{-# LANGUAGE OverloadedStrings #-}

module Create (createGame) where

import Network.Wreq
import qualified Network.Wreq.Session as Sess
import Control.Lens hiding ((.=))
import Data.Aeson (encode, object, (.=))
import Data.Aeson.Lens (key, _String)
import qualified Data.Text as T

createGame :: Sess.Session -> String -> IO ()
createGame sess playerId = do
    let url = "https://wordle.we4shakthi.in/game/create"
        payload = encode $ object ["id" .= playerId, "overwrite" .= True]
        opts = defaults & header "Content-Type" .~ ["application/json"]
    r <- Sess.postWith opts sess url payload
    let message = r ^? responseBody . key "message" . _String
        err     = r ^? responseBody . key "error" . _String
    case (message, err) of
        (Just msg, _) -> putStrLn $ "Game created: " ++ T.unpack msg
        (_, Just e)   -> putStrLn $ "Error: " ++ T.unpack e
        _             -> putStrLn $ "Unknown response."