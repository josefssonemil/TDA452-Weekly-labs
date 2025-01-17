module BlackJack where
import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)
import System.Random

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
value hand = if normalVal <= 21 then normalVal else valueWithValueOfAce 1 hand
  where normalVal = valueWithValueOfAce 11 hand


-- Checks the hand recursively by taking a card, finding its value and then
-- continues with the hand until empty
valueWithValueOfAce :: Integer -> Hand -> Integer
valueWithValueOfAce aV Empty = 0
valueWithValueOfAce aV (Add card hand) = valueCard aV card +
                       valueWithValueOfAce aV hand

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
winner handG handB | gameOver handG                      = Bank
                   | gameOver handB                      = Guest
                   | 21 - value handG < 21 - value handB = Guest
                   | otherwise                           = Bank


--B1
--Takes two hands and places the first one on top of the other hand
--Takes the secoond hand and then via recursion places the cards
--from the other hand
-- on top.

(<+) :: Hand -> Hand -> Hand
(<+) h1 Empty = h1
(<+) Empty h2 = h2
(<+) (Add card hand) h2 = Add card (hand <+ h2)

-- Property function, checks if (<+) operator is associative
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

-- Property function, checks that the size after adding two hands is
-- equal to the size when using the (<+) function
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1 <+ h2)

--B2

--Returns fulldeck in a Hand
fullDeck :: Hand
fullDeck = allSuit Spades <+ allSuit Diamonds <+ allSuit Hearts
                        <+ allSuit Clubs

--Return all cards with the given rank
allSuit :: Suit -> Hand
allSuit s = Empty <++ (Numeric 2,s) <++ (Numeric 3,s)
                  <++ (Numeric 4,s) <++ (Numeric 5,s)
                  <++ (Numeric 6,s) <++ (Numeric 7,s)
                  <++ (Numeric 8,s) <++ (Numeric 9,s)
                  <++ (Numeric 10,s) <++ (Jack,s)
                  <++ (Queen,s) <++ (King,s) <++ (Ace,s)


--Simple operator for adding a single card to clean up the allSuit
--function
(<++) :: Hand -> (Rank,Suit) -> Hand
(<++) h (r,s) = Add (Card r s) h

--B3
-- First input is Deck, second is the Hand.
-- Function draws the top card from the deck, places in the hand.
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty hand = error "draw: The deck is empty."
draw (Add card deck) hand = (deck, Add card hand)


--B4
-- Plays for the bank, given a hand and assuming initial hand is empty.
-- Returns the bank's final hand
playBank :: Hand -> Hand
playBank deck = snd (playBank' deck empty)

-- Helper function for playBank. Takes a deck and a hand, draws a card from
-- the deck to the hand and checks if value is 16 or over, repeats if it's
-- not.

playBank' :: Hand -> Hand -> (Hand,Hand)
playBank' deck bankHand = if value bankHand' >= 16 then
                        (deck',bankHand')
                        else playBank' deck' bankHand'
    where (deck', bankHand') = draw deck bankHand

-- B5
-- Shuffles a hand
shuffle :: StdGen -> Hand -> Hand
shuffle g Empty = Empty
shuffle g deck = snd (shuffleHelper g (deck,empty))

-- Helper function for shuffle. Moves all cards from first hand
-- to the second hand in random order.
-- First retrieves card number random from the first hand, and then places
-- it at the bottom of the second hand. Continues until the first hand
-- is empty
shuffleHelper :: StdGen -> (Hand,Hand) -> (Hand,Hand)
shuffleHelper g (h1,h2) = if h1' == empty then (h1' , Add c1' h2)
                          else shuffleHelper g' (h1' , Add c1' h2)
    where (n , g') = randomR (0, size h1 - 1) g
          (c1', h1') = getCard n h1



--Retrieves a card that is integer number from the top
--basically in order to not remove other cards the hole hand "shifts"
--integer many times and then returns the hand and card
getCard :: Integer -> Hand -> (Card,Hand)
getCard n Empty = error "getCard: empty deck"
getCard n hand | n < 0 || n > size hand = error "getCard: forbidden n"
getCard n (Add card hand) = if n == 0 then (card,hand)
                            else getCard (n - 1) (hand
                                 <+ Add card Empty)



--Helper function given in assignment, returns true if card
-- is in hand.
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- Property for shuffle function, makes sure no cards
-- are missing after shuffle.
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h

-- Property for shuffle function,
-- makes sure that the size is preserved
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g hand = size hand == size (shuffle g hand)

-- B6
implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffle
  }

main :: IO ()
main = runGame implementation
