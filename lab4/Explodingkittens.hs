module Explodingkittens where

import Cards
import System.Random
import Test.QuickCheck hiding (shuffle)

data Player = Player String
              deriving (Show, Eq)


-- Creates, depending on amount of players, a deck consisting of
-- player-dependent cards
createPlayDeck :: Integer -> Hand -> Hand
createPlaydeck n _ | n < 2 || n > 5  = error "Incorrect amount of players"
createPlayDeck n h = generateCards Kitten (n - 1) <+
                     generateCards Defuse 2 <+ h

-- Creates the standard deck containing all the cards needed
createStandardDeck :: Hand
createStandardDeck = generateCards Favor 4 <+ generateCards Skip 4 <+
                     generateCards Shuffle 4 <+ generateCards Future 5 <+
                     generateCards Catcard 5

-- Helper function which just generates cards recursively
generateCards :: Model -> Integer -> Hand
generateCards m n | n < 1 = Empty
generateCards m n  = Add (Card m) $ generateCards m (n-1)

-- Checks that the amount of generated cards corresponds with the wanted
-- amount
prop_generateCards_amountModel :: Model -> Integer -> Property
prop_generateCards_amountModel m i = i > 0 ==>
                                     handLength (generateCards m i) == i

-- Creates the starting hands for the players
createStartHands :: Integer -> Hand -> ([Hand],Hand)
createStartHands 0 deck = ([],deck)
createStartHands _ Empty = ([Empty],Empty)
createStartHands n deck = (hand : hands , deck'')
    where (deck', hand) = createStartHand deck
          (hands , deck'') = createStartHands (n-1) deck'

-- (Deck, Hand)
createStartHand :: Hand -> (Hand, Hand)
createStartHand deck = (deck' , Add (Card Defuse) hand)

          where (deck', hand) = draw deck Empty 7


-- Right shifting (rotating) a list with n steps
rotate :: Int -> [a] -> [a]
rotate n xs = drop k xs ++ take k xs
        where k = length xs - n


prop_rotate :: Eq a => Int -> [a] -> Bool
prop_rotate _ [] = True
prop_rotate 0 list = rotate 0 list == list
prop_rotate n list = length (rotate n list) == length list
                  && rotate (n-1) (rotate (n+1) list) == list
                  && rotate (length list) list == list
                  && head (rotate 1 list) == last list
-- Prints the hand as a (integer,model) list
-- ex card 0 is a defuse : (0, Model Defuse)
showHand :: Integer -> Hand -> [(Integer, Model)]
showHand n Empty = []
showHand n (Add (Card model) hand) = (n, model) : showHand (n + 1) hand

-- Prints the hand as a string
showHandString :: [(Integer, Model)] -> String
showHandString [] = ""
showHandString list = show (fst el) ++ ": " ++ show (snd el) ++ " " ++ "\n" ++ showHandString list'
                    where (el:list') = list

-- Operator that adds a hand to another one
(<+) :: Hand -> Hand -> Hand
(<+) h1 Empty = h1
(<+) Empty h2 = h2
(<+) (Add card hand) h2 = Add card (hand<+h2)

-- Returns the length of a hand
handLength :: Hand -> Integer
handLength h = handLength' h 0

handLength' :: Hand -> Integer -> Integer
handLength' Empty n = n
handLength' (Add card hand) n = handLength' hand (n+1)


prop_handLength :: Hand -> Bool
prop_handLength Empty = handLength Empty == 0
prop_handLength (Add card hand) = handLength hand + 1 ==
                                handLength (Add card hand)
                                && prop_handLength hand

-- Returns the size of a list as an Integer
sizeA :: [a] -> Integer
sizeA list = toInteger (length list)

-- Shuffles a hand (deck)
shuffle :: StdGen -> Hand -> Hand
shuffle g Empty = Empty
shuffle g hand = snd ( shuffleHelper g (hand,Empty) )

shuffleHelper :: StdGen -> (Hand,Hand) -> (Hand,Hand)
shuffleHelper g (h1 , h2) = if h1' == Empty then (h1' , Add c1 h2)
                            else shuffleHelper g' (h1' , Add c1 h2)
    where (n , g') = randomR (0, size h1-1) g
          (c1 , h1') = getCard n h1


-- Returns the card at the given index and the hand inputted
getCard :: Integer -> Hand -> (Card,Hand)
getCard n Empty = error "empty hand"
getCard n hand | n < 0 || n > handLength hand = error "too large hand"
getCard 0 (Add card hand) = (card,hand)
getCard n (Add card hand) = getCard (n-1) (hand <+ Add card Empty)

prop_getCard :: Integer -> Hand -> Bool
prop_getCard n hand | n < 0 || n > (handLength hand) = True
prop_getCard n Empty = True
prop_getCard _ (Add card hand) | hand == Empty = True
prop_getCard 0 (Add card hand) = fst (getCard 0 (Add card hand)) == card
prop_getCard n hand = handLength (snd(getCard (n-1) hand')) + 1
                    == handLength (snd(getCard n hand))
                    && prop_getCard (n-1) hand'
    where (Add card hand') = hand

-- Places a card at a given position in a given hand
placeCard :: Integer -> Card -> Hand -> Hand
placeCard n _ _ | n < 0 = error "Non allowed index"
placeCard n card deck = h1 <+ Add card h2
    where (h1,h2) = placeCard' n (Empty,deck)

placeCard' :: Integer -> (Hand,Hand) -> (Hand,Hand)
placeCard' n (_, Empty) = error "Deck empty"
placeCard' 0 (h1,h2) = (h1,h2)
placeCard' n (h1, Add card h2) = placeCard' (n-1) (Add card h1,h2)


--(Deck, hand), draws a card from the deck into the hand, returns
-- updated hand and deck
draw :: Hand -> Hand -> Integer -> (Hand,Hand)
draw Empty hand _ = (Empty,hand)
draw deck hand 0 = (deck,hand)
draw deck hand n = draw (snd drawn) (Add (fst drawn) hand) (n-1)
    where drawn = getCard 0 deck

-- Removes the first occurence of a card in a hand
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


-- Returns true of hand is empty
isEmpty :: Hand -> Bool
isEmpty hand | handLength hand == 0 = True
isEmpty hand | otherwise = False

-- Returns a list of models of the cards in a hand
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
          (c2,d2) = getCard (handLength hand - 1) hand
          (Card m1) = c1
          (Card m2) = c2


-- Retrieves a card, without deleting it, from a hand
retrieveCard :: Integer -> Hand -> Card
retrieveCard 0 (Add card hand) = card
retrieveCard n (Add card hand) = retrieveCard (n-1) hand

-- Returns true if hand holds a given card model
hasCard :: Card -> Hand -> Bool
hasCard c h = handLength h /= handLength h'

          where h' = removeCard c h


-- Plays the favor card: the other player must give his top card to the player
-- that played the defuse card
playFavor :: [(Player, Hand)] -> [(Player,Hand)]
playFavor playerHands = (p1, Add c h1) : (p2,h2') : playerHands''
    where ((p1,h1) : playerHands') = playerHands
          ((p2,h2) : playerHands'') = playerHands'
          (Add c h2') = h2

-- Call in game loop
-- Plays the shuffle card: shuffles the draw deck
playShuffle :: StdGen -> Hand -> Hand
playShuffle g hand = shuffle g hand


-- Plays the future card: player may view the top 3 cards in the draw deck
playFuture :: Hand -> Hand
playFuture Empty = Empty
playFuture deck = snd (draw hand Empty 3)
    where (deck',hand) = draw deck Empty 3


-- Shuffles the opponent hand and then takes a card from it
playCatcard :: StdGen -> Hand -> Hand -> (Hand,Hand)
playCatcard g h1 h2 = (snd drawn , fst drawn)
    where x = shuffle g h2
          drawn = draw x h1 1

-- Plays the nope card: stops the action of the other player
-- No time to implement before deadline
playNope :: Card -> Hand -> Hand
playNope = undefined

-- Plays the attack card:
-- No time to implement before deadline
playAttack :: Card -> Hand -> Hand
playAttack = undefined
