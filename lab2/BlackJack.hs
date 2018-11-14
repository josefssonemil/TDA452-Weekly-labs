module BlackJack where
import Cards
import RunGame

-- A0
-- Hand2 consists of Hearts 2 and Spades Jack + empty , which gives
-- 1 + Spades Jack + empty which gives 1 + 1 + 0 = 2

-- A1
-- Returns the empty hand
empty :: Hand
empty = Empty

-- A2
-- Takes a hand, compares it with different values of Ace (1 or 11).
-- It computes the initial value, and then the initial value by checking
-- if the initial value is larger than 21.
value :: Hand -> Integer
value hand = if valueWithValueOfAce 11 hand <= 21
             then valueWithValueOfAce 11 hand
             else valueWithValueOfAce 1 hand


-- Checks the hand recursively by taking a card, finding its value and then
-- continues with the hand until empty
valueWithValueOfAce :: Integer -> Hand -> Integer
valueWithValueOfAce aV Empty = 0
valueWithValueOfAce aV (Add card hand) = (valueCard aV card) +
                       (valueWithValueOfAce aV hand)

-- Returns the value of a rank
valueRank :: Integer -> Rank -> Integer
valueRank aV Jack = 10
valueRank aV Queen = 10
valueRank aV King = 10
valueRank aV Ace = aV
valueRank aV (Numeric x) = x

-- Returns the value of a card
valueCard :: Integer -> Card -> Integer
valueCard aceValue Card {rank=r} = valueRank aceValue r

-- A3
-- Checks if the game is over by comparing the value of the hand with 21
gameOver :: Hand -> Bool
gameOver hand = 21 < value hand

-- A4

-- Win conditions :
-- handG closer to 21 than handB, then handG wins
-- only handB bust, then handG wins
-- otherwise handB wins
-- We check this by finding the win conditions for the Guest, and if those
-- are not fulfilled, the bank wins
winner :: Hand -> Hand -> Player
winner handG handB
      | value handG <= 21 &&
      21 - value handG < 21 - value handB     = Guest
      | value handB > 21 && value handG <= 21 = Guest
      | otherwise                             = Bank

--B1

-- example hands for testing:
-- (Add (Card (Numeric 2) Hearts)(Add (Card Jack Spades) Empty))
-- (Add (Card Jack Spades)(Add (Card (Numeric 4) Diamonds) Empty))

--Takes two hands and places the first one on top of the other hand
--Takes the secoond hand and then via recursion places the casrds from the other hand
-- on top. 

--passes the tests below if done by hand, quickcheck did not want to work
(<+) :: Hand -> Hand -> Hand
(<+) h1 Empty = h1
(<+) Empty h2 = h2
(<+) (Add card hand) h2 = (Add card (hand <+ h2)) -- then  hand <+ h2 

-- Test calss from the lab directions
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

-- Test to see if size is corerct after <+ function
-- uses size from cards class
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1 <+ h2)






























