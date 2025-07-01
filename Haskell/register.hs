{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wreq
import Control.Lens hiding ((.=))
import Data.Aeson (encode, object, (.=))
import Data.Aeson.Lens (key, _String)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Default (def)
import Network.HTTP.Client (CookieJar, destroyCookieJar)

register :: String -> IO (String, CookieJar)
register name = do
    let url = "https://wordle.we4shakthi.in/game/register"
        payload = encode $ object ["mode" .= ("wordle" :: String), "name" .= name]
        opts = defaults
             & header "Content-Type" .~ ["application/json"]
    r <- postWith opts url payload
    let playerId = r ^? responseBody . key "id" . _String
        cookieJar = r ^. responseCookieJar
    return (maybe "No id found" T.unpack playerId, cookieJar)

main :: IO ()
main = do
    let playerName = "gnanika"
    (playerId, cookieJar) <- register playerName
    putStrLn $ "ID: " ++ playerId
    putStrLn $ "Session cookies: " ++ show (destroyCookieJar cookieJar)
