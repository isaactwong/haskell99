import Data.List
import System.Random
import Data.Ord (comparing)
import Data.Function (on)

-- Problem 1: Return the last element in a list.
myLast :: [a] -> a
myLast [] = error "Called myLast on an Empty List"
myLast [x] = x
myLast (x : xs) = myLast xs

-- Functional solution.
-- myLast = head . reverse

-- Problem 2: Find the last but one element in a list.
myButLast :: [a] -> a
myButLast [] = error "Called myButLast on an Empty List"
myButLast [x] = error "Called myButLast on an almost Empty List"
myButLast (x : [y]) = x
myButLast (x : xs) = myButLast xs

-- Functional solution.
-- myButLast = last . init

-- Problem 3: Find the kth element in a list
elementAt :: [a] -> Int -> a 
-- elementAt list k = list !! (k-1)

-- Functional solution.
-- elementAt x k = last (take k x)

-- Clever one from the web!
-- elementAt (x : _) 1 = x
-- elementAt (_ : xs) k = elementAt xs (k-1)

-- Another clever one from the web.
elementAt list k = (snd . last) $ zip [1..k] list

-- Problem 4: Find the number of elements in a list.
myLength :: [a] -> Int
myLength [x] = 1
myLength (x : xs) = 1 + myLength xs

-- Functional solution with zip.
-- myLength = fst . last . zip [1..]

-- Funtional solution with foldl
-- myLength = foldl (\sum _ -> sum + 1) 0

-- Problem 5: Reverse a list.
myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse (x : xs) = myReverse(xs) ++ [x]

-- Functional solution.
-- myReverse list = foldl (\reversed x -> [x] ++ reversed) [] list

-- Problem 6: Find out whether a list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = (reverse xs) == xs

-- Problem 7: 
data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List []) = []
myFlatten (List (x : xs)) = (myFlatten x) ++ (myFlatten (List xs))

-- Cool concatmap solution from the web.
-- myFlatten (Elem x) = [x]
-- myFlatten (List x) = concatMap myFlatten x

-- Problem 8: Eliminate consecutive duplicates of list elements
headMatch :: (Eq a) => a -> [a] -> Bool
headMatch x [] = False
headMatch x xs = ((head xs) == x)
myCompress :: (Eq a) => [a] -> [a]
myCompress xs = reverse $ foldl (\accum x -> if (headMatch x accum) then accum else (x : accum)) [] xs

-- Cool solution from the web.
-- myCompress xs = map head (group xs)

-- Problem 9: Pack consecutive duplicates of list elements into sublists
-- No cheating by simply calling group.
myPack :: (Eq a) => [a] -> [[a]]
myPack [] = []
myPack (x : xs) = (takeWhile (==x) (x : xs)) : (myPack (dropWhile (==x) xs))

-- Clever solution from the website.
-- myPack (x : xs) = let (first, rest) = span (==x) xs
                              -- in (x : first) : (myPack rest)

-- Problem 10: Run length encoding of a list.
myEncode :: (Eq a) => [a] -> [(Int, a)]
myEncode xs = map (\x -> (length x, head x)) (myPack xs)

-- Problem 11: Modified run length encoding of a list.
data OneOrMore a = Single a | Multiple Int a deriving (Show)
myEncodeModified :: (Eq a) => [a] -> [OneOrMore a]
myEncodeModified xs = map converter $ myEncode xs
                 where
                        converter (1, x) = Single x
                        converter (n, x) = Multiple n x

-- Problem 12: Decode a run-length string.
myDecode :: (Eq a) => [OneOrMore a] -> [a]
myDecode [] = []
myDecode (x : xs) = (decodeItem x) ++ (myDecode xs)
         where
                decodeItem (Single x) = [x]
                decodeItem (Multiple n x) = take n (repeat x)

-- Slick version from the website
-- myDecode xs = concatMap decodeItem xs
         -- where
                -- decodeItem (Single x) = [x]
                -- decodeItem (Multiple n x) = replicate n x

                
-- Problem 13: Run-length encoding of a list (direct solution). Based on website.
runningEncode :: (Eq a) => [a] -> [(Int, a)]
runningEncode xs = foldr processTerm [] xs
              where
                processTerm x [] = [(1,x)]
                processTerm x (y@(a,b) : ys)
                            | x == b = (1+a,x) : ys
                            | otherwise = (1, x) : y : ys
directEncode :: (Eq a) => [a] -> [OneOrMore a]
directEncode = map converter . runningEncode
             where
                converter (1, x) = Single x
                converter (n, x) = Multiple n x

-- Problem 14: Duplicate the elements of a list.
myDuplicate :: [a] -> [a]
myDuplicate [] = []
myDuplicate (x : xs) = x : x : (myDuplicate xs)

                
-- Problem 15: Replicate the elements of a list a given number of times.
myReplicate :: Int -> [a] -> [a]
myReplicate n [] = []
myReplicate n (x : xs) = (replicate n x) ++ myReplicate n xs

-- Slick ones from the web
-- myReplicate n xs = concatMap (replicate n) xs
-- myReplicate n xs = concatMap (take n . repeat) xs

-- Problem 16: Drop every N'th element from a list.
myDrop :: Int -> [a] -> [a]
myDrop n [] = []
myDrop n xs = (take (n-1) xs) ++ (myDrop n (drop n xs))


-- Problem 17: Split a list into 2 parts, the length of the first part if given.
mySplit :: Int -> [a] -> ([a], [a])
mySplit n xs = (take n xs, drop n xs)

-- Problem 18: Extact a slice from a list.
mySlice :: Int -> Int -> [a] -> [a]
mySlice n m = take (m-n+1) . drop (n-1)

-- Problem 19: Rotate a list N places to the left.
myRotate :: [a] -> Int -> [a]
myRotate xs n = if n >= 0 then
                   (drop n xs) ++ (take n xs)
                else
                   reverse(myRotate (reverse xs) (-n))

-- Problem 20: Remove the K-th element from a list.
myRemoveAt :: Int -> [a] -> (a, [a])
myRemoveAt n xs = (xs !! (n-1), (take (n-1) xs) ++ (drop n xs))

-- Problem 21: Insert an element at a give point into a list.
myInsertAt :: a -> [a] -> Int -> [a]
myInsertAt x xs n = take (n-1) xs ++ [x] ++ drop (n-1) xs

-- Problem 22: Create a list containing all integers within a given range.
myRange :: Int -> Int -> [Int]
myRange x y
        | x < y = drop x (take y [1..])
        | x == y = [y]
        | x > y = []

-- Slick one from the web.        
-- myRange x y
        -- | x == y = [y]
        -- | x < y = x : (myRange (x+1) y) 
        -- | x > y = []

-- Problem 23: Extract a given number of randomly selected elements from a list.
randomSelect :: [a] -> Int -> IO [a]
randomSelect xs n = do
  g <- getStdGen
  return (take n [xs !! x | x <- (randomRs (0, (length xs) - 1) g)])

-- Problem 24: Draw n different random numbers from the set 1..m.
lottoSelect :: Int -> Int -> IO [Int]
lottoSelect n m = do
  randomSelect [1..m] n

-- Problem 25: Generate a random permutation of the elements of a list.
randomPermutation :: [a] -> IO [a]
randomPermutation xs = do
  indices <- lottoSelect (length xs) (length xs)
  return [xs !! (n - 1) | n <- indices]

-- Another solution
-- randomPermutation xs = randomSelect xs (length xs)

-- Problem 26: Generate the combination of K distinct objects chosen from the N elemends of a list.
-- With a genius solution from the Haskell 99 website!
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [y:ys | y:xs1 <- tails xs, ys <- combinations (n-1) xs1]

-- Problem 27: Group the elements of a set into disjoint subsets
-- Another cool solution from Haskell 99 website.
combination :: Int -> [a] -> [([a], [a])]
combination 0 xs = [([], xs)]
combination n [] = []
combination n (x : xs) = ts ++ ds
            where
              ts = [(x:ys, zs) | (ys, zs) <- combination (n-1) xs]
              ds = [(ys, x:zs) | (ys, zs) <- combination n xs]
              
groupProb :: [Int] -> [a] -> [[[a]]]
groupProb [] _ = [[]]
groupProb (n:ns) xs = [g:gs | (g, rs) <- combination n xs, gs <- groupProb ns rs]

-- Problem 28: Sort a list according to length of subsets.
-- Again, great solution from Haskell 99!
-- Part a: Sort a list of lists according to the length of the sublists.
lengthSort :: [[a]] -> [[a]]
lengthSort = sortBy (comparing length)

-- Part b: Sort the elements according to their length frequency.
lengthFrequencySort :: [[a]] -> [[a]]
lengthFrequencySort = concat . lengthSort . groupBy (on (==) length) . lengthSort
