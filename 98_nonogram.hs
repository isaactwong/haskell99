import Data.List

{-
Around 1994, a certain kind of puzzle was very popular in England. The "Sunday Telegraph" newspaper wrote: "Nonograms are puzzles from Japan and are currently published each week only in The Sunday Telegraph. Simply use your logic and skill to complete the grid and reveal a picture or diagram." As a Prolog programmer, you are in a better situation: you can have your computer do the work! Just write a little program ;-).

The puzzle goes like this: Essentially, each row and column of a rectangular bitmap is annotated with the respective lengths of its distinct strings of occupied cells. The person who solves the puzzle must complete the bitmap given only these lengths.

             Problem statement:          Solution:
             |_|_|_|_|_|_|_|_| 3         |_|X|X|X|_|_|_|_| 3           
             |_|_|_|_|_|_|_|_| 2 1       |X|X|_|X|_|_|_|_| 2 1         
             |_|_|_|_|_|_|_|_| 3 2       |_|X|X|X|_|_|X|X| 3 2         
             |_|_|_|_|_|_|_|_| 2 2       |_|_|X|X|_|_|X|X| 2 2         
             |_|_|_|_|_|_|_|_| 6         |_|_|X|X|X|X|X|X| 6           
             |_|_|_|_|_|_|_|_| 1 5       |X|_|X|X|X|X|X|_| 1 5         
             |_|_|_|_|_|_|_|_| 6         |X|X|X|X|X|X|_|_| 6           
             |_|_|_|_|_|_|_|_| 1         |_|_|_|_|X|_|_|_| 1           
             |_|_|_|_|_|_|_|_| 2         |_|_|_|X|X|_|_|_| 2           
              1 3 1 7 5 3 4 3             1 3 1 7 5 3 4 3              
              2 1 5 1                     2 1 5 1                      

For the example above, the problem can be stated as the two lists [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]] and [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]] which give the "solid" lengths of the rows and columns, top-to-bottom and left-to-right, respectively. Published puzzles are larger than this example, e.g. 25 x 20, and apparently always have unique solutions.
-}

data Square = Blank | Cross deriving (Eq)
instance Show Square where
  show Blank = " "
  show Cross = "X"

rows :: Int -> [Int] -> [[Square]]
rows n [] = [replicate n Blank]
rows n (k:ks) | n < k = []
rows n (k:ks) =
  [Blank : row | row <- rows (n-1) (k:ks)] ++
  if (null ks) then [replicate k Cross ++ replicate (n-k) Blank] else [(replicate k Cross) ++ (Blank : row) | row <- rows (n-k-1) ks]

contract :: [Square] -> [Int]
contract xs = map length ys
  where ys = filter (\(x:_) -> x == Cross) (group xs)

solver :: [[Int]] -> [[Int]] -> [[[Square]]]
solver h v = filter fitsVertical possibleSolution
  where possibleSolution = mapM (rows (length v)) h
        fitsVertical rs = map contract (transpose rs) == v

nonogram :: [[Int]] -> [[Int]] -> [[Square]]
nonogram h v = head $ solver h v
