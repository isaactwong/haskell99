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

-- Haskell99 solutions.
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

{-
Problem 71

Determine the internal path length of a tree.

We define the internal path length of a multiway tree as the total sum of the path lengths from the root to all nodes of the tree. By this definition, tree5 has an internal path length of 9.
-}

internal_path_length :: (Tree a) -> Int
internal_path_length tree = ipl 0 tree
                     where
                        ipl n (Node y ts) = n + sum (map (ipl (n+1)) ts)

{-
Problem 72
Construct the bottom-up order sequence of the tree nodes.

Write a predicate bottom_up(Tree,Seq) which constructs the bottom-up sequence of the nodes of the multiway tree Tree.
-}

bottom_up :: (Show a) => (Tree a) -> [a]
bottom_up (Node x []) = [x]
bottom_up (Node x ts) = concat (map bottom_up ts) ++ [x]

{-
Problem 73
Lisp-like tree representation.

There is a particular notation for multiway trees in Lisp. Lisp is a prominent functional programming language, which is used primarily for artificial intelligence problems. As such it is one of the main competitors of Prolog. In Lisp almost everything is a list, just as in Prolog everything is a term.

The following pictures show how multiway tree structures are represented in Lisp.

Note that in the "lispy" notation a node with successors (children) in the tree is always the first element in a list, followed by its children. The "lispy" representation of a multiway tree is a sequence of atoms and parentheses '(' and ')', which we shall collectively call "tokens". We can represent this sequence of tokens as a Prolog list; e.g. the lispy expression (a (b c)) could be represented as the Prolog list ['(', a, '(', b, c, ')', ')']. Write a predicate tree_ltl(T,LTL) which constructs the "lispy token list" LTL if the tree is given as term T in the usual Prolog notation.
-}

lisp_tree :: (Tree Char) -> String
lisp_tree (Node x ts) = "(" ++ [x] ++ (concat (map f ts)) ++ ")"
          where
                f (Node y ys)
                  | length ys == 0 = [y]
                  | otherwise      = lisp_tree (Node y ys)