import Test.QuickCheck

-- Part 1

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- k = 0: 1 step
-- k = 1: 1 step + k(0)
-- k = 2: 1 step + k(1) + k(0)
-- k = 3: 1 step + k(2) + k(1) + k(0)
-- From this pattern, we can see that the amount of comupting
-- steps are k+1


-- Part 2
-- Uses the function product on a list containing n k times. Throws error if 
-- k is negative

power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n 0 = 1
power1 n k = product (replicate (fromIntegral k) n)


-- Part 3

-- Uses if/then/else to get a one liner as well as an 'even' check to know
-- which of the below equations to use.
--n^k = (n^2)^k/2 (k even)
--n^k = n * (n^k-1) (k odd)

power2 :: Integer -> Integer -> Integer 
power2 n k | k < 0 = error "power: negative argument"
power2 n 0  = 1
power2 n k = if even k then power2 (n*n) (div k 2) 
                            else (n * (power2 n (k-1)))    

-- Part 4

-- A
-- Test cases: k = 0, n = 0 & k = Integer, n = 0 & k = 0, n = integer & 
-- k = integer, n = integer

-- These test cases will cover all possible inputs and will make sure
-- nothing weird happens with certain inputs

-- B

-- Property function for all three different implementations of the
-- power function

prop_powers n k = (power n k) == (power1 n k) && (power n k) == (power2 n k)

-- C
-- Tuple list with all test cases from A, test_powers will test all cases
testCases n k = [(0,0), (n,0), (0,k), (n,k)]
test_powers n k = [prop_powers (fst x) (snd x) | x <- testCases n k]

--D

-- We do not allow negative numbers for the power functions,
-- thus we need to take the absolute value for each n and k.
prop_powers' n k = (power (abs n) (abs k)) == (power1 (abs n) (abs k))
 && (power (abs n) (abs k)) == (power2 (abs n) (abs k))

