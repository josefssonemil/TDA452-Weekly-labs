module BlackJack where
import Cards
import RunGame

-- A0
-- hand2 can be interpretted to kinda 2_hearts + (jack_spades + empty)
-- ... =  1 + size ( Add(card Jack Spades) empty
-- = 2 + size empty
-- = 2 + 0 = 2

-- A1
empty :: Hand
empty = Empty

-- A2

--Recurive thru the hand. Takes a card, finds its value and then continues
--with the rest of the hand untill its empty.
valueWithValueOfAce :: Integer -> Hand -> Integer
valueWithValueOfAce aV Empty = 0
valueWithValueOfAce aV (Add card hand) = (valueCard aV card) + 
                       (valueWithValueOfAce aV hand) 

-- Ace has AceValue here. 1 or 11
valueRank :: Integer -> Rank -> Integer
valueRank aV Jack = 10
valueRank aV Queen = 10
valueRank aV King = 10
valueRank aV Ace = aV
valueRank aV (Numeric x) = x

--Give valueRank the rank of the card and the aceValue
valueCard :: Integer -> Card -> Integer
valueCard aceValue (Card {rank=r}) = (valueRank aceValue r)
