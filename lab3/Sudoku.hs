
import Test.QuickCheck
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
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
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku matrix) = length matrix == 9 && isSudo' 0 matrix

-- Helper function, checks the length of each row recursivly
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

--Helper function. Prints the sudoku recursivly
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
helperReadSudoku string = Sudoku [[convertToMaybe ((lines string !! y)!! x)
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
-- Checks that a block doesn't contain the same number twice
isOkayBlock :: Block -> Bool
isOkayBlock block = length (nub block') == length block'
      where block' = filter (/=Nothing) block

-- * D2

-- Creates a list of all the blocks in the given sudoku
blocks :: Sudoku -> [Block]
blocks (Sudoku matrix) = blocks' matrix


-- Helper function, makes sure the blocks are formed correctly
blocks' :: [[Maybe Int]] -> [Block]
blocks' [] = []
blocks' matrix = sortToBlocks headmatrix ++ blocks' tailmatrix
    where headmatrix = take 3 matrix
          tailmatrix = drop 3 matrix

--Sorts the matrix into blocks according to the layout of a sudoku
sortToBlocks :: [[Maybe Int]] -> [Block]
sortToBlocks [] = []
sortToBlocks (list : matrix) = chunksOf 3 list <+ sortToBlocks matrix


-- adds two lists of same size, index by index
-- return lefthand side if not compatible
(<+) :: [[a]] -> [[a]] -> [[a]]
(<+) b1 b2 | length b1 /= length b2 = b1
(<+) b1 b2 = [b1 !! i  ++ b2 !! i | i <- [0..n]]
    where n = length b1 - 1

-- Makes sure that all blocks contains nine cells and whole sudoku has
-- nine blocks
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sudo = length (filter (\x -> 9 == length x)  blocky) == 9
    where blocky = blocks sudo

-- * D3

-- Check is Sudoku doesnt contain any duplicate values on its rows, columns or
-- blocks
isOkay :: Sudoku -> Bool
isOkay (Sudoku matrix) = isColumsAndRowsOkay matrix
                        && all isOkayBlock (blocks (Sudoku matrix))

-- Checks that all rows and columns does not contain any duplicates
-- using the IsOkayBlock function as it works not only for blocks but
-- for all [Maybe Int] type aswell.
isColumsAndRowsOkay :: [[Maybe Int]] -> Bool
isColumsAndRowsOkay matrix =  and $ map isOkayBlock matrix
                                 ++ map isOkayBlock matrix'
    where matrix' = transpose matrix




---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1
-- Returns a list of blank positions
blanks :: Sudoku -> [Pos]
blanks (Sudoku matrix) = blanks' matrix 0

-- Helper function, gives the actual list of blank positions recursivly
blanks' :: [[Maybe Int]] -> Int -> [Pos]
blanks' [] _  = []
blanks' (xs : xss) n = [ (n, snd(xs'' !! i)) | i <- [0..k]] ++
                        blanks' xss (n + 1)

          where xs' = xs `zip` [0..8]
                xs'' = filter (isNothing . fst) xs'
                k = length xs'' - 1


prop_blanks_allBlanks :: Sudoku -> Bool
prop_blanks_allBlanks (Sudoku matrix) = and [(matrix !! fst(list !! i))
                                        !! snd(list !! i) == Nothing
                                        | i <- [0..n]]

          where list = blanks (Sudoku matrix)
                n = length list - 1
-- * E2

-- Inserts the value given, at index i, in the given list
(!!=) :: [a] -> (Int,a) -> [a]
[] !!= _ = error "empty list"
xs !!= (i,y) | i > length xs || i < 0 = error "incorrect index"
xs !!= (i,y) = t1 ++ [y] ++ t2
      where t1 = fst(splitAt i xs)
            t2 = snd(splitAt (i + 1) xs)


-- Properties for (!!=) function
prop_bangBangEquals_correct :: Eq a => [a] -> (Int, a) -> Bool
prop_bangBangEquals_correct [] _ = True
prop_bangBangEquals_correct xs (i,y) | i < 0 || i > length xs = True
prop_bangBangEquals_correct xs (i,y) = (xs' !! i) == y &&
                                       (length xs == length xs'
                                       || (length xs +1 )  == length xs')


      where xs' = xs !!= (i,y)

-- * E3

-- Takes in a Sudoku, position and value and updates the Sudoku with the
-- new value at the given position
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sudoku _ Nothing = sudoku
update sudoku pos (Just n) | not $ isSudoku sudoku ||
                      fst pos > 8 || fst pos < 0 ||
                      snd pos > 8 || snd pos < 0 ||
                      n < 0 || n > 9 = error "invalid input"
update (Sudoku matrix) pos n  = Sudoku ( matrix !!= (fst pos, row'))
                    where row = matrix !! fst pos
                          row' = row !!= (snd pos, n)


prop_update_updated :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update_updated _ p _ | fst p < 0 || snd p < 0
                                        || fst p > 8 || snd p > 8 = True
prop_update_updated _ _ Nothing = True
prop_update_updated (Sudoku matrix) pos n = (matrix' !! fst pos)
                                            !! snd pos == n
    where (Sudoku matrix') = update (Sudoku matrix) pos n


-- * E4

-- Generates candidates that can be legally inserted in a given
-- position in a Sudoku
candidates :: Sudoku -> Pos -> [Int]
candidates (Sudoku matrix) (x,y) | isJust ((matrix !! x) !! y) = []

candidates sudo pos = filter (isOkayPlay sudo pos) [1..9]



isOkayPlay :: Sudoku -> Pos -> Int -> Bool
isOkayPlay (Sudoku matrix) (x,y) n = isOkayBlock row
                                  && isOkayBlock col
                                  && isOkayBlock block'
    where blkidx = 3 * mod x 3 + mod y 3
          row = (matrix !! x) !!= (x, Just n)
          col = (transpose matrix !! y) !!= (y, Just n)
          block = getBlock (blocks (Sudoku matrix)) (x,y)
          block' = block !!= (blkidx, Just n)


getBlock :: [Block] -> Pos -> Block
getBlock blocks (i,j) = blocks !! index
    where i' = div i 3
          j' = div j 3
          index =  (3*i') + j'



prop_candidates_correct :: Sudoku -> Pos -> Bool
prop_candidates_correct sudo (x,y) | x < 0 || y < 0
                                   || isSudoku sudo = True
prop_candidates_correct sudo pos =
          elem pos (blanks sudo) &&
          length filtered == length cands
    where cands = candidates sudo pos
          isOK = (\x -> isOkay (update sudo pos (Just x)))
          filtered = filter isOK cands



------------------------------------------------------------------------------

-- * F1
-- Attempts to solve a given Sudoku
solve :: Sudoku -> Maybe Sudoku
solve sudo | not (isSudoku sudo && isOkay sudo) = Nothing
solve sudo = solve' sudo

-- Helper function for solve, which does the actual work
solve' :: Sudoku -> Maybe Sudoku
solve' sudo | null (blanks sudo) = Just sudo
solve' sudo = listToMaybe (filter isOkay
                               (catMaybes (map solver candi)))
    where blks = blanks sudo
          candi = candidates sudo (head blks)
          solver = (\x -> solve' (update sudo (head blks) (Just x)))

-- * F2

-- Takes a filepath, reads the Sudoku, solves it and then prints it
readAndSolve :: FilePath -> IO ()
readAndSolve filepath = do { sud <- readSudoku filepath
                        ; printSudoku (fromJust (solve sud)) }

-- * F3


-- Checks whether sud1 is a solution of sud2
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sud1 sud2 | not (isOkay sud1) || not (isFilled sud1) = False
isSolutionOf sud1 sud2 = checkPositions 0 sud1 sud2


-- Checks if a position contains the same number in both Sudokus
checkPositions :: Int -> Sudoku -> Sudoku -> Bool
checkPositions i sud1 sud2 | i == length (filterBlanks sud2) - 1 = True
checkPositions i sud1 sud2 = getNumber sud1 k
                            == getNumber sud2 k
                            && checkPositions (i + 1) sud1 sud2

          where posList = filterBlanks sud2
                k = posList !! i

-- Filters out all blank positions in a Sudoku
filterBlanks :: Sudoku -> [Pos]
filterBlanks sud = filter (`notElem` blankPos) pos

            where blankPos = blanks sud
                  pos = [ (x,y) | x <- [0..8], y <- [0..8]]

-- Gets the number that is in a given position in a Sudoku
getNumber :: Sudoku -> Pos -> Maybe Int
getNumber (Sudoku matrix) pos = (matrix !! x) !! y

          where x = fst pos
                y = snd pos


-- * F4
-- Checks that every supposed solution produced by solve actually
-- is a valid solution of the original problem
-- We check that the solved sudoku is on the form isJust, otherwise
-- it isn't solved. We bind that boolean condition to the property,
-- namely that the solved sudoku should be a solution of itself
prop_solveSound :: Sudoku -> Property
prop_solveSound sudoku = isJust solvSudoku ==>
                        isSolutionOf (fromJust solvSudoku) sudoku

            where solvSudoku = solve sudoku

-------------------------------------------------------------------------
