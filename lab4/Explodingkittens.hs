module Explodingkittens where

import Cards
import System.Random

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

(<+) :: Hand -> Hand -> Hand
(<+) h1 Empty = h1
(<+) Empty h2 = h2
(<+) (Add card hand) h2 = Add card (hand<+h2)

handLength :: Hand -> Integer
handLength h = handLength' h 0

handLength' :: Hand -> Integer -> Integer
handLength' Empty n = n
handLength' (Add card hand) n = handLength' hand (n+1)

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
   

-- Player plays a card. Chooses the following function depending
-- on card chosen

-- TODO: fix the input/output in functions 
-- so they correspond with each action card. Needs to be discussed,
-- current types are just placeholders for now

playCard :: Card -> Hand -> Hand
playCard (Card model) hand = undefined
                 --   | model == Skip = playSkip (Card model) hand
                 --   | model == Defuse = playDefuse (Card model) hand
                  --  | model == Favor = playFavor (Card model) hand
                   -- | model == Future = playFuture (Card model) hand
                   -- | model == Catcard = playCatcard (Card model) hand
                   -- | model == Nope = playNope (Card model) hand
                   -- | model == Attack = playAttack (Card model) hand



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
playFavor :: Integer -> Hand -> Hand -> (Hand,Hand)
playFavor n h1 h2 = (Add (fst cardNhand) h1 , (snd cardNhand))
    where cardNhand = getCard n h2

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
          drawn = draw x h1 0  

-- Plays the nope card: stops the action of the other player
playNope :: Card -> Hand -> Hand
playNope = undefined

-- Plays the attack card: ??
playAttack :: Card -> Hand -> Hand
playAttack = undefined
