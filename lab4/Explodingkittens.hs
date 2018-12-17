import Cards

createFirstDeck :: Integer -> Hand
createFirstDeck n = undefined

createSecondDeck :: Integer -> Hand -> Hand  
createSecondDeck = undefined

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
