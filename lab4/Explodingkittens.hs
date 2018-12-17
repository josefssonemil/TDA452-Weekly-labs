import Cards
import System.Random
-- Integer = Amount of players
createPlayDeck :: Integer -> Hand -> Hand
createPlaydeck n _ | n < 2 || n > 5  = error "Incorrect amount of players"
createPlayDeck n h = generateCards Kitten (n-1) <+
                     (generateCards Defuse 2) <+ h

createStandardDeck :: Hand
createStandardDeck = generateCards Favor 4 <+ generateCards Skip 4 <+
                     generateCards Shuffle 4 <+ generateCards Nope 5 <+
                     generateCards Future 5 <+ generateCards Attack 4 <+
                     generateCards Catcard 16

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

-- Player plays a card. Chooses the following function depending
-- on card chosen
playCard :: Card -> Hand -> (Card, Hand)
playCard (Card model) hand
                    | model == Skip = playSkip (Card model) hand
                    | model == Defuse = playDefuse (Card model) hand
                    | model == Favor = playFavor (Card model) hand
                    | model == Shuffle = playShuffle (Card model) hand
                    | model == Future = playFuture (Card model) hand
                    | model == Catcard = playCatcard (Card model) hand
                    | model == Nope = playNope (Card model) hand
                    | model == Attack = playAttack (Card model) hand

-- Plays the defuse card: If player draws an exploding kitten, this card
-- can be used to prevent dying. The player will then put the Exploding Kitten
-- back into the pile wherever he wants

-- Input: defuse card and players hand
-- Output: players new hand and new deck
playDefuse :: Card -> Hand -> (Hand, Hand)
playDefuse card hand =

playSkip :: Card -> Hand -> (Card, Hand)
playSkip = undefined

playFavor :: Card -> Hand -> (Card, Hand)
playFavor = undefined

playShuffle :: Card -> Hand -> (Card, Hand)
playShuffle = undefined

playFuture :: Card -> Hand -> (Card, Hand)
playFuture = undefined

playCatcard :: Card -> Hand -> (Card, Hand)
playCatcard = undefined

playNope :: Card -> Hand -> (Card, Hand)
playNope = undefined

playAttack :: Card -> Hand -> (Card, Hand)
playAttack = undefined
