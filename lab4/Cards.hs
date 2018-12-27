module Cards where
import Test.QuickCheck


-- Much of this is taken from the previous Blackjack lab
-- A card does only consist of a type (model)
data Card = Card {model :: Model}
            deriving (Show, Eq)

-- Type of card (this should also hold the Attack and Nope cards, but they
-- are not implemented)
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

type Hand = [Card]

--instance Arbitrary Hand where
--  arbitrary = frequency [(1, return [])
--                       , (10, do card <- arbitrary
  --                               hand <- arbitrary
    --                             return (card : hand))]

size :: Num a => [a] -> a
size [] = 0
size (x:xs) = 1 + size xs
