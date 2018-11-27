
import Test.QuickCheck
import Data.Char
import Data.List
import Data.List.Split
-------------------------------------------------------------------------

-- | Representation of sudoku puzzlese (allows some junk)
data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 $ replicate 9 n)
  where n = Nothing


-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
--Passes all the examples but in reality only checks if matrix contains
--9 lists and that the first list in matrix aslo has 9 elements
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku matrix) = length matrix == 9 && isSudo' 0 matrix

isSudo' :: Int -> [[Maybe Int]] -> Bool
isSudo' 8 matrix = length (matrix !! 8) == 9
isSudo' n matrix = length (matrix !! n) == 9 && isSudo' (n + 1) matrix


-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku xs) =  not $ or [Nothing `elem` x | x <- xs ]


-------------------------------------------------------------------------

-- * B1

-- |b printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
-- does nothing if not valid sudoku
printSudoku :: Sudoku -> IO ()
printSudoku sudo | not $ isSudoku sudo = return()
printSudoku sudo | otherwise = printSudoku' sudo

--helper function. Prints the sudoku recursivly
printSudoku' :: Sudoku -> IO ()
printSudoku' (Sudoku []) = return ()
printSudoku' (Sudoku (x:xs)) = do { putStrLn (map toSudoChar x)
                          ;printSudoku' (Sudoku xs) }

--Takes a Maybe and outputs a char. A simple '.' if nothing
-- otherwise its value
toSudoChar :: Maybe Int -> Char
toSudoChar Nothing = '.'
toSudoChar (Just x) =  intToDigit x


-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku filepath = do  text <- readFile filepath
                          return (helperReadSudoku text)

-- Helper function for readSudoku
-- Creates the [[Maybe Int]] matrix from the text
helperReadSudoku :: String -> Sudoku
helperReadSudoku string = Sudoku [[convertToMaybe (((lines string) !! y)!! x)
                                 | x <- [0..8] ]
                                 | y <- [0..8] ]

--Takes a char and translates it to a Maybe Int
convertToMaybe :: Char -> Maybe Int
convertToMaybe '.' = Nothing
convertToMaybe c = Just (digitToInt c)

-------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = do n <- choose (1,9)
          frequency [(9, return Nothing),(1, return (Just n))]
-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)




-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sudo = isSudoku sudo

------------------------------------------------------------------------------

type Block = [Maybe Int]


-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock block = length (nub block') == length block'

      where block' = filter (/=Nothing) block

-- * D2

blocks :: Sudoku -> [Block]
blocks sudo = blocks' sudo 0


blocks' :: Sudoku -> Int -> [Block]
blocks' _ 3 = []
blocks' (Sudoku matrix) n = blockHelper matrix'
    where matrix' = take 3 $ drop (0) matrix

blockHelper :: [[Maybe Int]] -> [Block]
blockHelper matrix = [[(matrix !! i) !! j | i <- [0..2]] | j <- [0..2]]
--blockHelper list = (list !! 0) <+ (list !! 1) <+ (list !! 2)

-- possibly could be improved to be more general
-- adds two lists of same size, index by index
(<+) :: [Block] -> [Block] -> [Block]
(<+) b1 b2 | length b1 /= length b2 = error "different sizes"
(<+) b1 b2 = [b1 !! i  ++ b2 !! i | i <- [0..n]]
    where n = (length b1) - 1

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths = undefined

-- * D3

isOkay :: Sudoku -> Bool
isOkay = undefined


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


-- * E4

candidates :: Sudoku -> Pos -> [Int]
candidates = undefined

--prop_candidates_correct :: ...
--prop_candidates_correct =


------------------------------------------------------------------------------

-- * F1


-- * F2

-- * F3


-- * F4

-------------------------------------------------------------------------
