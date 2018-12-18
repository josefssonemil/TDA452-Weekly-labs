module Cards where
import Test.QuickCheck


-- Much of this is taken from the previous Blackjack lab
-- A card does only consist of a type (model)
data Card = Card {model :: Model}
            deriving (Show, Eq)

-- Type of card
data Model = Kitten | Skip | Favor | Shuffle | Future |
            Catcard | Defuse
            deriving (Show, Eq)

-- Generating arbitary cards
instance Arbitrary Card where
 arbitrary = do model <- arbitrary
                return (Card model)

-- Generating arbitary types
instance Arbitrary Model where
         arbitrary = oneof [return Kitten, return Skip, return Favor,
                           return Shuffle ,return Future, return Catcard,
                           return Defuse]

data Hand = Empty | Add Card Hand
            deriving (Show , Eq)

instance Arbitrary Hand where
  arbitrary = frequency [(1, return Empty)
                       , (10, do card <- arbitrary
                                 hand <- arbitrary
                                 return (Add card hand))]

size :: Num a => Hand -> a
size Empty = 0
size (Add card hand) = 1 + size hand
