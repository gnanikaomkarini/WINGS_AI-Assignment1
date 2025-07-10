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

guessAPI :: Sess.Session -> String -> String -> IO (String, String, Maybe String)
guessAPI sess playerId guessWord = do
    let url = "https://wordle.we4shakthi.in/game/guess"
        payload = encode $ object ["guess" .= guessWord, "id" .= playerId]
        opts = defaults & header "Content-Type" .~ ["application/json"]
    result <- try (Sess.postWith opts sess url payload) :: IO (Either SomeException (Response BL8.ByteString))
    case result of
        Right r -> do
            let feedback = r ^? responseBody . key "feedback" . _String
                message  = r ^? responseBody . key "message" . _String
                answer   = r ^? responseBody . key "answer" . _String
            return (maybe "" T.unpack feedback, maybe "" T.unpack message, fmap T.unpack answer)
        Left _ -> do
            -- Try to extract answer and message from the exception's response body if possible
            -- But since we can't, just return a special marker
            return ("EXCEEDED", "You have exceeded the number of allowed guesses.", Nothing)
