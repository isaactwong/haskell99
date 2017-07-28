import Data.List
import Data.Array
import Control.Monad

{-
Anyway the puzzle goes like this: Given a tree with N nodes (and hence N-1 edges). Find a way to enumerate the nodes from 1 to N and, accordingly, the edges from 1 to N-1 in such a way, that for each edge K the difference of its node numbers equals to K. The conjecture is that this is always possible.

For small trees the problem is easy to solve by hand. However, for larger trees, and 14 is already very large, it is extremely difficult to find a solution. And remember, we don't know for sure whether there is always a solution!

Write a predicate that calculates a numbering scheme for a given tree. What is the solution for the larger tree pictured below?

Again, Haskell99 to the rescue with a cool backtracking solution.
-}

edges1 = [(1, 5), (1, 6), (1, 7), (2, 5), (2, 6), (2, 8), (3, 5), (3, 7), (3, 8), (4, 6), (4, 7), (4, 8)]
edges2 = [(1,6),(2,6),(3,6),(4,6),(5,6),(5,7),(5,8),(8,9),(5,10),(10,11),(11,12),(11,13),(13,14)]

vonKoch :: [(Int, Int)] -> [[Int]]
vonKoch edges = do
    let n = (length edges) + 1
    nodes <- permutations [1..n]
    -- nodeArray is a re-numbering of the edges that were supplied. For example,
    -- the permutation (3,4,2,1) means 1->3, 2->4 etc. The listArray makes this
    -- concrete by creating (1,3), (2,4), ...
    let nodeArray = listArray (1,n) nodes
    -- Find all the identifiers for the new permuted edges, remembering that
    -- we want the edge identifiers to be the difference of the nodes. Sort this
    -- array to bring identical identifiers, if they exist, next to each other.
    let dists = sort $ map (\(x,y) -> abs (nodeArray ! x - nodeArray ! y)) edges
    -- Make sure none of the edge numbers are identical.
    -- And backtrack for more potential solutions.
    guard $ and $ zipWith (/=) dists (tail dists)
    -- Return the permutations of the edges that pass the test.
    return nodes
