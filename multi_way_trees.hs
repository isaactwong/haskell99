import Data.List

{-
Problem 70B
Check whether a give term represents a multiway tree.

In Haskell this is represented by a data type and doesn't require predicates.
-}

data Tree a = Node a [Tree a] deriving (Eq, Show)

tree1 = Node 'a' []
tree2 = Node 'a' [Node 'b' []]
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
tree5 = Node 'a' [
                    Node 'f' [Node 'g' []],
                    Node 'c' [],
                    Node 'b' [Node 'd' [], Node 'e' []]
                 ]

{-
Problem 70C

Count the nodes of a multiway tree

-}

nnodes :: Tree a -> Int
nnodes (Node t xs) = 1 + sum (map nnodes xs)


{-
Problem 70
We suppose that the nodes of a multiway tree contain single characters. In the depth-first order sequence of its nodes, a special character ^ has been inserted whenever, during the tree traversal, the move is a backtrack to the previous level.

By this rule, the tree below (tree5) is represented as: afg^^c^bd^e^^^


Define the syntax of the string and write a predicate tree(String,Tree) to construct the Tree when the String is given. Make your predicate work in both directions.
-}

treeToString :: (Tree Char) -> String
treeToString (Node x []) = [x]
treeToString (Node x ts) = [x] ++ (concat $ intersperse "^" (map treeToString ts)) ++ "^"

stringToTree :: String -> (Tree Char)
stringToTree (x:xs) = (Node x (fst (convert xs)))
             where
                convert [] = ([],"")
                convert (x:xs)
                        | x == '^'  = ([], xs)
                        | otherwise = ([(Node x) trees0] ++ trees1, rest1)
                                    where (trees0, rest0) = convert xs
                                          (trees1, rest1) = convert rest0