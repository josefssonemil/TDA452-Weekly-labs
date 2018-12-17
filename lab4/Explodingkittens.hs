import Cards
import System.Random
-- Integer = Amount of players
createPlayDeck :: Integer -> Hand -> Hand  
createPlaydeck n _ | n < 2 || n > 5  = error "Incorrect amount of players"
createPlayDeck n h = (genarateCards Kitten (n-1)) <+
                     (genarateCards Defuse 2) <+ h

createStandardDeck :: Hand
createStandardDeck = (genarateCards Favor 4) <+ (genarateCards Skip 4) <+
                     (genarateCards Shuffle 4) <+ (genarateCards Nope 5) <+
                     (genarateCards Future 5) <+ (genarateCards Attack 4) <+
                     (genarateCards Catcard 16)

genarateCards :: Model -> Integer -> Hand 
genarateCards m n | n < 1 = Empty
genarateCards m n  = Add (Card m) $ genarateCards m (n-1) 

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



