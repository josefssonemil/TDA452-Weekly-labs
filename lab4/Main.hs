module Main where

import Cards
import Explodingkittens
import System.Random
import Test.QuickCheck hiding (shuffle)
import Data.Char

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
           putStrLn "Everyone is here. Lets play!!"
           gameLoop playerHands deck'''

printPlayerHand :: (Player,Hand) -> IO()
printPlayerHand (p,h) = do let x = showHand 0 h
                           let y = showHandString x
                           let str = "Its " ++ (getPlayerName p) ++ "s turn!"
                           let str1 = "Press [n] to skip playing a card"
                           putStrLn (str ++ "\n" ++ y ++ str1) 


gameLoop :: [(Player,Hand)] -> Hand -> IO()
gameLoop playerHands deck | length playerHands == 1 =
                            winner (fst (head playerHands))
gameLoop playerHands deck = do
                            printPlayerHand current
                            n <- getChar
                            let k =  toInteger (digitToInt k)
                            if n == "n" then putStrLn "Did not play anything"
                            else do let gameInfo = playCard k playerHands deck
                                    let deck' = snd gameInfo 
                                    let playerHands'' = fst gameInfo
                            

          where playerHands' = rotate 1 playerHands
                current = head playerHands'


--Current player is head of tuple list
playCard :: Integer -> [(Player, Hand)] -> Hand -> ([(Player,Hand)], Hand)
playCard k playerHands deck =  



 

winner :: Player -> IO()
winner (Player name) = putStrLn ("Winner is: " ++ name)

main :: IO ()
main = start
