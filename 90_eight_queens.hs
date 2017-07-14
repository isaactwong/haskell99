{-
This is a classical problem in computer science. The objective is to place eight queens on a chessboard so that no two queens are attacking each other; i.e., no two queens are in the same row, the same column, or on the same diagonal.

Hint: Represent the positions of the queens as a list of numbers 1..N. Example: [4,2,7,3,6,8,5,1] means that the queen in the first column is in row 4, the queen in the second column is in row 2, etc. Use the generate-and-test paradigm.

Haskell99 saves me again with a much better solution.
-}

queens :: Int -> [[Int]]
queens n = filter test (generatePositions n n)
  where test []     = True
        test (q:qs) = isSafe q qs && test qs

generatePositions :: Int -> Int -> [[Int]]
generatePositions 0 _ = [[]]
generatePositions m k = [(c:cs) | c <- [1..k], cs <- generatePositions (m-1) k]

isSafe :: Int -> [Int] -> Bool
isSafe try qs = not (elem try qs || onDiaganol try qs)

-- This is nifty. Basically takes y2-y1/x2-x1 = m, let m = 1. This depends on the
-- zip function indexing the y-coordinates with the distance from the current position
-- that we are testing to see if it is safe.
onDiaganol :: Int -> [Int] -> Bool
onDiaganol try qs = any (\(colDist, q) -> abs (try-q) == colDist) $ zip [1..] qs

  -- Clever solution from Haskell99. Much faster.
queens1 :: Int -> [[Int]]
queens1 n = map reverse $ queens' n
    where queens' 0       = [[]]
          queens' k       = [q:qs | qs <- queens' (k-1), q <- [1..n], isSafe q qs]
