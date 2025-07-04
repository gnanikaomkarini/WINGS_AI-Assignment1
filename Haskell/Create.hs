{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wreq
import Control.Lens hiding ((.=))
import Data.Aeson (encode, object, (.=))
import Data.Aeson.Lens (key, _String)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL8
import Network.HTTP.Client (CookieJar)
import Register (register) 

createGame :: String -> CookieJar -> IO ()
createGame playerId cookieJar = do
    let url = "https://wordle.we4shakthi.in/game/create"
        payload = encode $ object ["id" .= playerId, "overwrite" .= True]
        opts = defaults
             & header "Content-Type" .~ ["application/json"]
             & cookies .~ Just cookieJar
    r <- postWith opts url payload
    let created = r ^? responseBody . key "created"
        message = r ^? responseBody . key "message" . _String
        err     = r ^? responseBody . key "error" . _String
    case (created, message, err) of
        (Just _, Just msg, _) -> putStrLn $ "Game created: " ++ T.unpack msg
        (_, _, Just e)        -> putStrLn $ "Error: " ++ T.unpack e
        _                     -> putStrLn $ "Unknown response: " ++ BL8.unpack (r ^. responseBody)

main :: IO ()
main = do
    (playerId, cookieJar) <- register "gnanika"
    putStrLn $ "Registered with ID: " ++ playerId
    putStrLn "Creating game..."
    createGame playerId cookieJar