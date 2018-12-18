module Explodingkittens where

import Cards
import System.Random
import Test.QuickCheck hiding (shuffle)

data Player = Player String
              deriving (Show, Eq)
-- Integer = Amount of players
createPlayDeck :: Integer -> Hand -> Hand
createPlaydeck n _ | n < 2 || n > 5  = error "Incorrect amount of players"
createPlayDeck n h = generateCards Kitten (n-1) <+
                     generateCards Defuse 2 <+ h

--only 8 catcard, modified rules
createStandardDeck :: Hand
createStandardDeck = generateCards Favor 4 <+ generateCards Skip 4 <+
                     generateCards Shuffle 4 <+ generateCards Nope 5 <+
                     generateCards Future 5 <+ generateCards Attack 4 <+
                     generateCards Catcard 8

generateCards :: Model -> Integer -> Hand
generateCards m n | n < 1 = Empty
generateCards m n  = Add (Card m) $ generateCards m (n-1)

--createStartHands :: [Player] -> Hand -> ([(Player, Hand)], Hand)
--createStartHands player:players deck = ((player, hand)
  --                                   : fst (createStartHands players deck')
    --                                  , )
      --           where (deck',hand) = createStartHand deck

createStartHands :: Integer -> Hand -> ([Hand],Hand)
createStartHands 0 deck = ([],deck)
createStartHands _ Empty = ([Empty],Empty)
createStartHands n deck = (hand : hands , deck'')
    where (deck', hand) = createStartHand deck
          (hands , deck'') = createStartHands (n-1) deck'

-- Deck then hand
createStartHand :: Hand -> (Hand, Hand)
createStartHand deck = (deck' , Add (Card Defuse) hand)

          where (deck', hand) = draw deck Empty 7


rotate :: Int -> [a] -> [a]
rotate n xs = drop k xs ++ take k xs
        where k = length xs - n

showHand :: Integer -> Hand -> [(Integer, Model)]
showHand n Empty = []
showHand n (Add (Card model) hand) = (n, model) : showHand (n + 1) hand

showHandString :: [(Integer, Model)] -> String
showHandString [] = ""
showHandString list = show (fst el) ++ ": " ++ show (snd el) ++ " " ++ "\n" ++ showHandString list'
                    where (el:list') = list

(<+) :: Hand -> Hand -> Hand
(<+) h1 Empty = h1
(<+) Empty h2 = h2
(<+) (Add card hand) h2 = Add card (hand<+h2)

handLength :: Hand -> Integer
handLength h = handLength' h 0

handLength' :: Hand -> Integer -> Integer
handLength' Empty n = n
handLength' (Add card hand) n = handLength' hand (n+1)

sizeA :: [a] -> Integer
sizeA list = toInteger (length list)

shuffle :: StdGen -> Hand -> Hand
shuffle g Empty = Empty
shuffle g hand = snd ( shuffleHelper g (hand,Empty) )

shuffleHelper :: StdGen -> (Hand,Hand) -> (Hand,Hand)
shuffleHelper g (h1 , h2) = if h1' == Empty then (h1' , Add c1 h2)
                            else shuffleHelper g' (h1' , Add c1 h2)
    where (n , g') = randomR (0, size h1-1) g
          (c1 , h1') = getCard n h1


getCard :: Integer -> Hand -> (Card,Hand)
getCard n Empty = error "empty hand"
getCard n hand | n < 0 || n > handLength hand = error "too large hand"
getCard 0 (Add card hand) = (card,hand)
getCard n (Add card hand) = getCard (n-1) (hand <+ Add card Empty)

--Deck then hand
draw :: Hand -> Hand -> Integer -> (Hand,Hand)
draw Empty hand _ = (Empty,hand)
draw deck hand 0 = (deck,hand)
draw deck hand n = draw (snd drawn) (Add (fst drawn) hand) (n-1)
    where drawn = getCard 0 deck

--remove first occorence of card in hand
removeCard :: Card -> Hand -> Hand
removeCard c Empty = Empty
removeCard c h = removeCard' (handLength h) c h

removeCard' :: Integer -> Card -> Hand -> Hand
removeCard' n c Empty = Empty
removeCard' 0 _ hand = hand
removeCard' n (Card m1) hand = if m1 == m2 then h
                               else removeCard' (n-1) (Card m1)
                                    (h <+ Add (Card m2) Empty)
    where (Add (Card m2) h) = hand

isEmpty :: Hand -> Bool
isEmpty hand | handLength hand == 0 = True
isEmpty hand | otherwise = False

-- Needs to be in same order as the hand for possible indexing
getModelList :: Hand -> [Model]
getModelList Empty = []
getModelList (Add (Card model) hand) = model : getModelList hand

prop_modelList_test :: Hand -> Bool
prop_modelList_test Empty = True
prop_modelList_test h | handLength h < 2 = True
prop_modelList_test hand = (handLength hand) == (toInteger (length models)) &&
                           (last models) == m2 && (head models) == m1
    where models = getModelList hand
          (c1,d1) = getCard 0 hand
          (c2,d2) = getCard ((handLength hand)-1) hand
          (Card m1) = c1
          (Card m2) = c2

-- Player plays a card. Chooses the following function depending
-- on card chosen

-- TODO: fix the input/output in functions
-- so they correspond with each action card. Needs to be discussed,
-- current types are just placeholders for now

--playCard :: Card -> Hand -> Hand
--playCard (Card model) hand = undefined
                 --   | model == Skip = playSkip (Card model) hand
                 --   | model == Defuse = playDefuse (Card model) hand
                  --  | model == Favor = playFavor (Card model) hand
                   -- | model == Future = playFuture (Card model) hand
                   -- | model == Catcard = playCatcard (Card model) hand
                   -- | model == Nope = playNope (Card model) hand
                   -- | model == Attack = playAttack (Card model) hand


retrieveCard :: Integer -> Hand -> Card
retrieveCard 0 (Add card hand) = card
retrieveCard n (Add card hand) = retrieveCard (n-1) hand


--Current player is head of tuple list
playCard :: Integer -> [(Player, Hand)] -> Hand -> ([(Player,Hand)], Hand)
playCard k playerHands deck = playCard' (retrieveCard k h) playerHands deck
          where ((p,h) : playerHands') = playerHands

playCard' :: Card -> [(Player, Hand)] -> Hand -> ([(Player,Hand)], Hand)
playCard' (Card m) playerHands deck | m == Favor = playFavor playerHands'' deck
                                    | m == Skip = ([], Empty)
                                    | m == Defuse = ([], Empty)
                                    | m == Attack = ([], Empty)
                                    | m == Catcard = ([], Empty)
                                    | m == Nope = ([], Empty)
                                    | m == Future = ([], Empty)
                                    | otherwise = ([], Empty)

            where (p,h) : playerHands' = playerHands
                  h' = removeCard (Card m) h
                  playerHands'' = (p,h'):playerHands'

hasCard :: Card -> Hand -> Bool
hasCard c h = handLength h /= handLength h'

          where h' = removeCard c h


-- Plays the defuse card: If player draws an exploding kitten, this card
-- can be used to prevent dying. The player will then put the Exploding Kitten
-- back into the pile wherever he wants

-- Input: defuse card and players hand
-- Output: players new hand and new deck
playDefuse :: Card -> Hand -> Hand
playDefuse = undefined

-- Plays the skip card: players turn ends without having to draw a new card
-- Input: player hand and card
-- Output: player hand without skip card
playSkip :: Card -> Hand -> Hand
playSkip = undefined

-- Needs IO in gamelloop
-- Plays the favor card: the other player must choose a card to give to
-- the player that played the favor card
-- first current players hand then opponent
-- gets top card for now
playFavor :: [(Player, Hand)] -> Hand -> ([(Player,Hand)], Hand)
playFavor playerHands deck = (( (p1,(Add c h1)) : (p2,h2') : playerHands''),deck)
    where ((p1,h1) : playerHands') = playerHands
          ((p2,h2) : playerHands'') = playerHands'
          (Add c h2') = h2

-- Call in game loop
-- Plays the shuffle card: shuffles the draw deck
playShuffle :: StdGen -> Hand -> Hand
playShuffle g hand = shuffle g hand

-- Takes deck, gives top 3 cards
-- Plays the future card: player may view the top 3 cards in the draw deck
playFuture :: Hand -> Hand
playFuture Empty = Empty
playFuture deck = snd( draw deck Empty 3)

-- Needs fixing
-- Start with only 1 catcard
-- Current first then opponent
playCatcard :: StdGen -> Hand -> Hand -> (Hand,Hand)
playCatcard g h1 h2 = (snd drawn , fst drawn)
    where x = shuffle g h2
          drawn = draw x h1 1

-- Plays the nope card: stops the action of the other player
playNope :: Card -> Hand -> Hand
playNope = undefined

-- Plays the attack card: ??
playAttack :: Card -> Hand -> Hand
playAttack = undefined
