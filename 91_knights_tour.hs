import Data.List
import Data.Ord (comparing)

{-
Another famous problem is this one: How can a knight jump on an NxN chessboard in such a way that it visits every square exactly once? A set of solutions is given on the The_Knights_Tour page.

Hints: Represent the squares by pairs of their coordinates of the form X/Y, where both X and Y are integers between 1 and N. (Note that '/' is just a convenient functor, not division!) Define the relation jump(N,X/Y,U/V) to express the fact that a knight can jump from X/Y to U/V on a NxN chessboard. And finally, represent the solution of our problem as a list of N*N knight positions (the knight's tour).

Find a tour ending at a particular square

Thanks to Haskell99 for making mine fast. sortImage is clever.
-}

type Square = (Int, Int)

moves :: Int -> Square -> [Square]
moves n (x,y) = filter (legalPosition n) [(x+a,y+b) | (a,b) <- [(1,2), (-1,2), (-2,1), (-2,-1), (-1,-2), (1,-2), (2,-1), (2,1)]]

legalPosition :: Int -> Square -> Bool
legalPosition n (x,y) = 1 <= x && x <= n && 1 <= y && y <= n

tourEndingAt :: Int -> Square -> [[Square]]
tourEndingAt n end = [(pos:path) | (pos,path) <- tour (n*n)]
  where tour 1 = [(end, [])]
        tour m = [(p', p:ps) |
                   (p,ps) <- tour (m-1),
                   p' <- sortImage (entrances ps) (filter (`notElem` ps) (moves n p))]
        entrances ps p = length (filter (`notElem` ps) (moves n p))

-- Sort by comparing the image of list elements under a function f.
-- These images are saved to avoid recomputation.
sortImage :: Ord b => (a -> b) -> [a] -> [a]
sortImage f xs = map snd (sortBy cmpFst [(f x, x) | x <- xs])
  where cmpFst = comparing fst



