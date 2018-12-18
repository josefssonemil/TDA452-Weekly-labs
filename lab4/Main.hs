module Main where

import Cards
import Explodingkittens
import System.Random
import Test.QuickCheck hiding (shuffle)
import Data.Char
import Control.Monad



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
printPlayerHand (_,Empty) = putStrLn "Empty hand"
printPlayerHand (p,h) = do let x = showHand 0 h
                           let y = showHandString x
                           let str = "Its " ++ (getPlayerName p) ++ "s turn!"
                           let str1 = "Press [n] to skip playing a card"
                           putStrLn (str ++ "\n" ++ y ++ str1)

endTurnAndDraw :: [(Player,Hand)] -> Hand -> IO()
endTurnAndDraw playerHands deck = do
  let drawnCard = draw deck (snd current) 1
  let newDeck = fst drawnCard
  let newHand = snd drawnCard
  let (p,h): playerHands' = playerHands
  let playerHands'' = (p,newHand) : playerHands'

  let (Card model) = retrieveCard 0 newHand
  when (model == Kitten) $ do
    when (not (hasCard (Card Defuse) (snd drawnCard))) $ do
      let updatedPlayers = tail playerHands''
      putStrLn ("Player " ++  getPlayerName (fst(head (playerHands))) ++  " died" ++ "\n")
      gameLoop updatedPlayers deck
    useDefuseCard playerHands'' newDeck



  gameLoop (rotate 1 playerHands'') newDeck
      where current = head playerHands

useDefuseCard :: [(Player,Hand)] -> Hand -> IO()
useDefuseCard playerHands deck = do
                        let newHand = removeCard (Card Defuse) hand
                        let newHand' = removeCard (Card Kitten) newHand
                        let ((p,h) :playerHands') = playerHands
                        let playerHands'' = (p,newHand'):playerHands'
                        let string = "Select position to put kitten: \n" ++ "From 0 -> "
                        putStrLn (string ++ (show (handLength deck - 1)))
                        r <- getLine
                        let n = read r :: Integer
                        let newDeck = placeCard n (Card Kitten) deck
                        putStrLn "Used defuse card to survive kitten!"
                        gameLoop playerHands'' newDeck

          where hand = snd (head playerHands)

playCard :: Integer -> [(Player,Hand)] -> Hand -> IO()
playCard k playerHands deck = do
      let (Card m) = retrieveCard k (snd current)
      let removed = removeCard (Card m) (snd current)
      let (p,h): playerHands' = playerHands
      let newPlrHds = (p,removed) : playerHands'
      when (m == Favor) $ do
        putStrLn "Favor card played"
        let playerHands'' = playFavor newPlrHds
        gameLoop playerHands'' deck
      when (m == Shuffle) $ do
        g <- newStdGen
        let deck' = playShuffle g deck
        putStrLn "Shuffling deck"
        gameLoop newPlrHds deck'
      when (m == Future) $ do
        putStrLn "Future card played \n"
        let topCards = playFuture deck
        putStrLn ("Top three cards: \n" ++ showHandString (showHand 0 topCards))
        gameLoop newPlrHds deck
      when (m == Skip) $ do
        putStrLn "Skip card played \n"
        gameLoop (rotate 1 newPlrHds) deck
      when (m == Catcard) $ do
        putStrLn "Catcard played \n"
        g <- newStdGen
        let (h1,h2) = playCatcard g removed (snd (last (newPlrHds)))
        let ((p1,h1') : newPlrHds') = newPlrHds
        let ((p2,h2') : newPlrHds'') = newPlrHds'
        let updatedPlayHands = (p1,h1) : (p2,h2) : newPlrHds''
        gameLoop updatedPlayHands deck
      when (m == Defuse) $ do
        let ((p1,h1) : newPlrHds') = newPlrHds
        let newPlrHds'' = (p1, Add (Card Defuse) h1) : newPlrHds'
        gameLoop newPlrHds'' deck
        
    where current = head playerHands

gameLoop :: [(Player,Hand)] -> Hand -> IO()
gameLoop playerHands deck | length playerHands == 1 =
                            winner (fst (head playerHands))
                          | handLength (snd (head playerHands)) == 0
                            = endTurnAndDraw playerHands deck
gameLoop playerHands deck = do
                            printPlayerHand current
                            n <- getLine
                            let k = read n :: Integer
                            putStrLn "\n"
                            --let k =  toInteger (digitToInt n)
                            if head n == 'n' then endTurnAndDraw playerHands deck
                              else playCard k playerHands deck
          where current = head playerHands








winner :: Player -> IO()
winner (Player name) = putStrLn ("Winner is: " ++ name)

main :: IO ()
main = start
