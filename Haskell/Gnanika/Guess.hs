{-# LANGUAGE OverloadedStrings #-}

module Guess (guessAPI) where

import Network.Wreq
import qualified Network.Wreq.Session as Sess
import Control.Lens hiding ((.=))
import Data.Aeson (encode, object, (.=))
import Data.Aeson.Lens (key, _String)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL8
import Control.Exception (try, SomeException)
import ApiConfig

guessUrl :: String
guessUrl = baseUrl ++ "guess"

guessAPI :: Sess.Session -> String -> String -> IO (String, String, Maybe String)
guessAPI sess playerId guessWord = do
    let url = guessUrl
        payload = encode $ object ["guess" .= guessWord, "id" .= playerId]
    result <- try (Sess.postWith opts sess url payload) :: IO (Either SomeException (Response BL8.ByteString))
    case result of
        Right r -> do
            let feedback = r ^? responseBody . key "feedback" . _String
                message  = r ^? responseBody . key "message" . _String
                answer   = r ^? responseBody . key "answer" . _String
            return (maybe "" T.unpack feedback, maybe "" T.unpack message, fmap T.unpack answer)
        Left _ -> return ("EXCEEDED", "You have exceeded the number of allowed guesses.", Nothing)
