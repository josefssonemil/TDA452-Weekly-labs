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
-- k is negative and will only accept k as an int.
power1 :: Double -> Int -> Double
power1 n k | k < 0 = error "power: negative argument"
power1 n 0 = 1
power1 n k = product (take k (repeat n))


-- Part 3
-- Integer instead of Int because we are not using the function product
-- n can be anything, but k has to be an Integer
-- Uses if/then/else to get a one liner
--n^k = (n^2)^k/2 (k even)
--n^k = n * (n^k-1) (k odd)
power2 :: Double -> Integer -> Double 
power2 n k | k < 0 = error "power: negative argument"
power2 n 0  = 1
power2 n k = if even k then power2 (n*n) (div k 2) 
                            else (n * (power2 n (k-1)))    

-- Part 4
