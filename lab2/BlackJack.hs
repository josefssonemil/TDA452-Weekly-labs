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
