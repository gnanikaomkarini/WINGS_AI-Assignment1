{-# LANGUAGE OverloadedStrings #-}

module Register (register) where

import Network.Wreq
import qualified Network.Wreq.Session as Sess
import Control.Lens hiding ((.=))
import Data.Aeson (encode, object, (.=))
import Data.Aeson.Lens (key, _String)
import qualified Data.Text as T

register :: Sess.Session -> String -> IO (Maybe String)
register sess name = do
    let url = "https://wordle.we4shakthi.in/game/register"
        payload = encode $ object ["mode" .= ("wordle" :: String), "name" .= name]
        opts = defaults & header "Content-Type" .~ ["application/json"]
    r <- Sess.postWith opts sess url payload
    let playerId = r ^? responseBody . key "id" . _String
    return (fmap T.unpack playerId)
