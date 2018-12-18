module Main where

import Cards
import Explodingkittens
import System.Random
import Test.QuickCheck hiding (shuffle)

data Player = Player String
              deriving (Show, Eq)

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
           g2 <- newStdGen
           let deck = shuffle g createStandardDeck
           let (hands , deck') = createStartHands (sizeA players) deck
           let playerHands = zip players hands
           let deck'' = createPlayDeck (sizeA players) deck'
           let deck''' = shuffle g2 deck''
           putStrLn "Start Screen Over. Lets play!!"
           gameLoop playerHands deck'''



gameLoop :: [(Player,Hand)] -> Hand -> IO()
gameLoop playerHands deck | length playerHands == 1 =
                            winner (fst (head playerHands))
gameLoop playerHands deck = do
                            let currentHand = showHand 0 (snd current)
                            putStrLn (show(fst current) ++ " \n" ++ showHandString currentHand)

          where playerHands' = rotate 1 playerHands
                current = head playerHands



winner :: Player -> IO()
winner (Player name) = putStrLn ("Winner is: " ++ name)

main :: IO ()
main = start
