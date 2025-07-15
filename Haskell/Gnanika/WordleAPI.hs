{-# LANGUAGE OverloadedStrings #-}

module Main where

import Register (register)
import Create (createGame)
import Guess (guessAPI)
import qualified Network.Wreq.Session as Sess
import System.IO
import Control.Monad (when)
import Data.Char (toLower)
import Data.List (elemIndices, (\\), nub)
import System.Random (randomRIO)
import Network.Wreq (get)

instructions :: String
instructions = "The computer will guess the word. Hehehe"

getWords :: IO [String]
getWords = do
    content <- readFile "medium.txt"
    return [map toLower w | w <- lines content, length w == 5]

getFeedback :: String -> String -> String
getFeedback guess answer =
    let feedback = zipWith (\g a -> if g == a then 'G' else 'R') guess answer
        answerChars = zipWith (\g a -> if g == a then Nothing else Just a) guess answer
        guessChars  = zipWith (\g a -> if g == a then Nothing else Just g) guess answer
        yellow = [if Just g /= Nothing && Just g `elem` answerChars then 'Y' else f | (g, f) <- zip guess feedback]
    in yellow

removeImpossibleWords :: [String] -> String -> String -> [String]
removeImpossibleWords words lastGuess lastFeedback =
    let
        greens = [if f == 'G' then Just c else Nothing | (c, f) <- zip lastGuess lastFeedback]
        ambers = [c | (c, f) <- zip lastGuess lastFeedback, f == 'Y']
        greys  = [c | (c, f) <- zip lastGuess lastFeedback, f == 'R']

        matchGreens w = and [maybe True (== wc) g | (wc, g) <- zip w greens]

        matchAmbers w = all (\a -> a `elem` w) ambers
                      && and [not (f == 'Y' && w !! i == lastGuess !! i) | (i, f) <- zip [0..] lastFeedback]

        matchGreys w = all (\g -> g `notElem` w) (nub greys \\ ambers)
    in
        filter (\w -> matchGreens w && matchAmbers w && matchGreys w) words

playRound :: Sess.Session -> String -> [String] -> String -> Int -> IO (Bool, [String], String)
playRound sess pid possibleWords guessWord attempts = do
    putStrLn guessWord
    (fb, msg, mAnswer) <- guessAPI sess pid guessWord
    if "exceeded" `elem` words (map toLower msg)
      then do
        putStrLn "Game over! You have exceeded the number of allowed guesses."
        case mAnswer of
            Just ans -> putStrLn $ "The answer was: " ++ ans
            Nothing  -> return ()
        return (True, possibleWords, fb)
    else if fb == "GGGGG"
      then do
        putStrLn $ "Wowee! The computer guessed We 4 Shakti's in " ++ show (attempts + 1) ++ " attempts!"
        return (True, possibleWords, fb)
    else return (False, possibleWords, fb)

gameLoop :: Sess.Session -> String -> [String] -> IO ()
gameLoop sess pid wordsList = do
    putStrLn instructions
    let maxAttempts = 6
    let loop ws guessWord attempts status lastResponse = do
            if status == "WON"
                then return ()
            else if attempts >= maxAttempts
                then putStrLn "Darn it!! The computer couldn't guess your word."
            else if null ws
                then putStrLn "No more possible words. Game over."
            else do
                (won, _, fb) <- playRound sess pid ws guessWord attempts
                if won
                    then return ()
                else do
                    let newWords = removeImpossibleWords (tail ws) guessWord fb
                    let nextGuess = if null newWords then "" else head newWords
                    loop newWords nextGuess (attempts + 1) (if won then "WON" else "PLAY") fb
    let firstGuess = head wordsList
    loop wordsList firstGuess 0 "PLAY" ""

main :: IO ()
main = do
    sess <- Sess.newSession
    pid <- register sess "gnanika"
    if null pid
      then putStrLn "Registration failed."
      else do
        createGame sess pid
        wordsList <- getWords
        let playLoop = do
                gameLoop sess pid wordsList
                putStr "Do you want to play again? (y/n): "
                hFlush stdout
                again <- getLine
                when (map toLower again == "y") $ do
                    createGame sess pid
                    playLoop
        playLoop