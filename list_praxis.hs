import Data.List
-- Problem 31: Determine whether a given integer is prime
-- Using the sieve of Eratosthenes
allPrimesToN :: Integer -> [Integer]
allPrimesToN n = eratosthenes [2..n]
             where 
                   eratosthenes [] = []
                   eratosthenes (x:xs) = x : (eratosthenes (xs \\ [x, x+x..n]))

isPrime :: Integer -> Bool
isPrime n = (last . allPrimesToN) n == n

-- Problem 32: Determine the greatest common divisor of two positive integers
gcdPraxis :: Integer -> Integer -> Integer
gcdPraxis n m 
     | m == 0 = (abs n)
     | otherwise = gcdPraxis m (mod n m)

-- Problem 33: Determine whether two positive integers are coprime.
coprime :: Integer -> Integer -> Bool
coprime m n = gcdPraxis m n == 1

-- Problem 34: Calculate Euler's totient function phi(m)
totientPhi :: Integer -> Integer
totientPhi n = foldr (\a acc -> if (coprime n a) then acc + 1 else acc) 0 [1..n]
-- Very cool and concise answer from Haskell99
-- totientPhi n = length [x | x <- [1..n], coprime n x]

-- Problem 35: Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
primeFactors :: Integer -> [Integer]
primeFactors n = ((filter isPrime) . (filter isDivisor)) [2..n]
             where isDivisor m = mod n m == 0

-- Problem 36: Determine the prime factors of a given positive integer with their multiplicity
-- Cool trick from Haskell99
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n = map encode ((group . pFactors) n)
                 where encode xs = (head xs, length xs)

pFactors :: Int -> [Int]
pFactors n = pFactors' n 2
         where
          pFactors' 1 _ = []
          pFactors' n f
                    | (mod n f) == 0 = f : pFactors' (div n f) f
                    | otherwise = pFactors' n (f+1)
