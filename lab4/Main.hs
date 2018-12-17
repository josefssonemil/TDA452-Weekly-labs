module Main where

import Cards
import Explodingkittens
import System.Random
import Test.QuickCheck hiding (shuffle)

data Player = Player String
              deriving Show

addNewPlayers :: IO [Player]
addNewPlayers = addNewPlayers'

addNewPlayers' :: IO [Player]
addNewPlayers' =      do   player <- addNewPlayer
                           putStrLn "Add another player? [y]"
                           n <- getLine
                           if (n == "y") then do
                             players <- addNewPlayers'
                             return (player : players)
                           else
                             return (player : [])

                           --if (player /= Empty)
                           --else return list

addNewPlayer :: IO Player
addNewPlayer = do putStrLn "Set player name: "
                  name <- getLine
                  return (Player name)


getPlayerName :: Player -> String
getPlayerName (Player name) = name

start :: IO ()
start = do putStrLn "Welcome to Exploding Kittens. Make your choice:"
           players <- addNewPlayers
           putStrLn "Players: "
           let names = map getPlayerName players
           putStrLn (unlines names)

           g <- newStdGen
           let deck = shuffle g createStandardDeck
           putStrLn "Start Screen Over. Lets play!!"
           --let playerhands = zip players hands 


--playLoop :: IO [(Player,Hand)] -> Hand -> IO()







