import Data.List
-- Problem 31: Determine whether a given integer is prime
-- Using the sieve of Eratosthenes
allPrimesToN :: Integer -> [Integer]
allPrimesToN n = eratosthenes [2..n]
             where 
                   eratosthenes [] = []
                   eratosthenes (x:xs) = x : (eratosthenes (xs \\ [x, x+x..n]))

isPrime :: Integer -> Bool
isPrime 1 = False
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

-- Problem 37: Calculate Eulers totient function phi(m) (improved).
eulerPhi :: Int -> Int
eulerPhi n = foldr (*) 1 $ map (\(p, m) -> (p-1)*p^(m-1)) $ primeFactorsMult n

-- Another succint one from Haskell99
-- eulerPhi n = product [(p-1)*p^(m-1) | (p,m) <- primeFactorsMult n]

-- Problem 38: Compare the 2 methods of calculating Euler's totient function.
-- 37 is better! Though for 10090 there wasn't much difference in performance to the naked eye. But the memory and allocation was vastly different.
-- ghc -prof -fprof-auto -rtsopts arithmetic_praxis.hs
-- ./arithmetic_praxis +RTS -p

-- Problem 39: A list of prime numbers in a specified integer interval.
primeRange :: Integer -> Integer -> [Integer]
primeRange n m = filter isPrime [n..m]

-- Problem 40: Goldbach's conjecture: Every positive integer greater than 2 is the sum of two numbers. For a given integer, find the two primes that sum to that number.
goldbach :: Integer -> (Integer, Integer)
goldbach n = head (filter (\(a,b) -> isPrime a && isPrime b) ((zip [2..n] (map ((-) n) [2..n]))))

-- And of course, another cool solution from Haskell99
-- goldbach n = head [(a,b) | a <- primeRange 2 n, b <- primeRange 2 n, a + b == n]
