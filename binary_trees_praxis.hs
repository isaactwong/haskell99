-- Problem 54A: Check whether a given term represents a binary tree
-- Done! Haskell's type system ensures this for us! Yeah Haskell!

-- Definition of a binary tree.
data BinaryTree a = Empty |  Branch a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

-- Random tree of Chars
tree1 = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
                               (Branch 'e' Empty Empty))
                   (Branch 'c' Empty
                               (Branch 'f' (Branch 'g' Empty Empty)
                                           Empty))
-- A binary tree consisting of a root node only
tree2 = Branch 'a' Empty Empty
 
-- An empty binary tree
tree3 = Empty

-- A tree of integers
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )


-- Problem 55: Construct completely balanced binary trees.
-- Binary trees are defined to be balanced if |#-nodes-left-subtree - #-nodes-right-subtree| <= 1
-- More help from the website. This one is magic.
balancedBinaryTrees :: Int -> [BinaryTree Char]
balancedBinaryTrees 0 = [Empty]
balancedBinaryTrees n = let (q, r) = quotRem (n-1) 2
                    in [Branch 'x' left right | i <- [q..(q+r)],
                       left <- balancedBinaryTrees i,
                       right <- balancedBinaryTrees (n-i-1)]

-- Problem 56: Symmetric binary trees.
-- Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Write a predicate symmetric/1 to check whether a given binary tree is symmetric. Hint: Write a predicate mirror/2 first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.
isMirror :: BinaryTree Char -> BinaryTree Char -> Bool
isMirror Empty Empty = True
isMirror (Branch _ left1 right1) (Branch _ left2 right2) = (left1 == right2) && (right1 == left2)
isMirror _ _ = False

-- The website recurses with isMirror
-- isMirror (Branch _ left1 right1) (Branch _ left2 right2) =  isMirror left1 right2 && isMirror right1 left

isSymmetric :: BinaryTree Char -> Bool
isSymmetric Empty = True
isSymmetric (Branch _ left right) = isMirror left right

-- Problem 57: Binary Search Trees
-- Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to construct a binary search tree from a list of integer numbers.
construct :: (Ord a) => [a] -> BinaryTree a
construct xs = foldl addToTree Empty xs
          where addToTree Empty x = Branch x Empty Empty
                addToTree (Branch y left right) x =
                          if (x <= y)
                             then Branch y (addToTree left x) right
                             else Branch y left (addToTree right x)

-- Problem 58: Generate and Test Paradigm
-- Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.
symBalancedBTrees :: Int -> [BinaryTree Char]
symBalancedBTrees = filter isSymmetric . balancedBinaryTrees

-- Problem 59
-- Construct height-balanced binary trees
-- In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.
-- Construct a list of all height-balanced binary trees with the given element and the given maximum height.
heightBalTree :: Int -> [BinaryTree Char]
heightBalTree 0 = [Empty]
heightBalTree 1 = [Branch 'x' Empty Empty]
heightBalTree n =
              [Branch 'x' left right | (l,r) <- [(n-2, n-1), (n-1, n-1), (n-1, n-2)],
              left <- heightBalTree l, right <- heightBalTree r]

-- Problem 60: Construct height-balanced binary trees with a given number of nodes
-- Naive approach is way too slow!
heightBalancedTree :: Int -> [BinaryTree Char]
heightBalancedTree n = filter (\tree -> (countNodes tree) == n) (heightBalancedTree n)

-- Problem 61: Count the leaves of a binary tree.
countNodes :: BinaryTree a -> Int
countNodes Empty = 0
countNodes (Branch x left right) = 1 + (countNodes left) + (countNodes right)

-- Problem 61A: Collect the leaves of a binary tree in a list.
-- A leaf is a node with no successor
leaves :: BinaryTree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch x left right) = (leaves left) ++ (leaves right)

-- Problem 62: Collect the internal nodes of a binary tree in a list
-- An internal node of a binary tree has either one or two non-empty successors. Write a predicate internals/2 to collect them in a list.
internals :: BinaryTree a -> [a]
internals Empty = []
internals (Branch x Empty Empty) = []
internals (Branch x left right) = [x] ++ (leaves left) ++ (leaves right)

-- Problem 62b
-- Collect the nodes at a given level in a list
-- A node of a binary tree is at level N if the path from the root to the node has length N-1. The root node is at level 1. Write a predicate atlevel/3 to collect all nodes at a given level in a list.
-- Instead of counting from 1->n and keeping two variables to track target and where you are, we start counting at n and then decrease until we are at 1. This means we only need one Int. Solution from Haskell99 page and much better then the ascending count I did first.
atLevel :: BinaryTree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch x left right) target
        | target == 1 = [x]
        | target > 1  = (atLevel left (target-1)) ++ (atLevel right (target-1))
        | otherwise   = []

-- Problem 63
-- Construct a complete binary tree
-- A complete binary tree with height H is defined as follows:
-- The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2**(i-1) at the level i)
-- In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted". This means that in a levelorder tree traversal all internal nodes come first, the leaves come second, and empty successors (the nil's which are not really nodes!) come last.
-- Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.
-- We can assign an address number to each node in a complete binary tree by enumerating the nodes in level-order, starting at the root with number 1. For every node X with address A the following property holds: The address of X's left and right successors are 2*A and 2*A+1, respectively, if they exist. This fact can be used to elegantly construct a complete binary tree structure.
-- Write a predicate complete_binary_tree/2.
-- Not sure I quite understand the question here. Don't they want a full tree to the specified level? Not sure why we aren't filling in the right branches.
completeBinaryTree :: Int -> BinaryTree Char
completeBinaryTree n = buildCBTree 1
  where buildCBTree h
          | h > n     = Empty
          | otherwise = Branch 'x' (buildCBTree (2*h+1)) (buildCBTree (2*h+1))  

-- Problem 64
-- In this layout strategy, the position of a node v is obtained by the following two rules:
-- x(v) is equal to the position of the node v in the inorder sequence
-- y(v) is equal to the depth of the node v in the tree
-- Write a function to annotate each node of the tree with a position, where (1,1) in the top left corner or the rectangle bounding the drawn tree.
-- Again, with tons of help from Haskell99.
type Pos = (Int, Int)
layout :: BinaryTree a -> BinaryTree (a, Pos)
layout tree = fst (parse_tree tree 1 1)
       where parse_tree Empty x y = (Empty, x)
             parse_tree (Branch a left right) x y = (Branch (a, (x', y)) left' right', x'')
                        where (left', x')   = parse_tree left x (y+1)
                              (right', x'') = parse_tree right (x'+1) (y+1)

-- Problem 65
-- An alternative layout method is depicted in the illustration below:
-- See https://wiki.haskell.org/99_questions/Solutions/65
-- Find out the rules and write the corresponding function. Hint: On a given level, the horizontal distance between neighboring nodes is constant.
-- Use the same conventions as in problem P64 and test your function in an appropriate w
-- Again, saved by the smart people at Haskell99.
layout2 :: BinaryTree a -> BinaryTree (a, Pos)
layout2 tree = layout_aux x1 1 sep1 tree
        where d = depth tree
              ld = left_depth tree
              x1 = 2^(d-1) - 2^(d-ld) + 1
              sep1 = 2^(d-2)
              layout_aux x y sep Empty = Empty
              layout_aux x y sep (Branch a left right) =
                         Branch (a, (x,y))
                                (layout_aux (x-sep) (y+1) (div sep 2) left)
                                (layout_aux (x+sep) (y+1) (div sep 2) right)

-- Depth of a tree. The depth of a branch point is the max of the left or right branch + 1.
depth :: BinaryTree a -> Int
depth Empty = 0
depth (Branch a left right) = (max (depth left) (depth right)) + 1

left_depth :: BinaryTree a -> Int
left_depth Empty = 0
left_depth (Branch a left right) = (left_depth left) + 1

-- Problem 66
-- The method yields a very compact layout while maintaining a certain symmetry in every node. Find out the rules and write the corresponding Prolog predicate. Hint: Consider the horizontal distance between a node and its successor nodes. How tight can you pack together two subtrees to construct the combined binary tree?
-- See https://wiki.haskell.org/99_questions/Solutions/66
-- Use the same conventions as in problem P64 and P65 and test your predicate in an appropriate way. Note: This is a difficult problem. Don't give up too early!
--I gave up! Again, help from Haskell99 saved me here!
layout3 :: BinaryTree a -> BinaryTree (a, Pos)
layout3 tree = tree'
        where (left, tree', right) = layout_aux x1 1 tree
              x1 = (maximum left)+1 -- how is this possible?

layout_aux :: Int -> Int -> BinaryTree a -> ([Int], BinaryTree (a, Pos), [Int])
layout_aux x y Empty = ([], Empty, [])
layout_aux x y (Branch a left right) = (ll', Branch (a, (x,y)) l' r', rr')
           where (ll, l', lr) = layout_aux (x-sep) (y+1) left
                 (rl, r', rr) = layout_aux (x+sep) (y+1) right
                 sep = (div (maximum (0:zipWith (+) lr rl)) 2) + 1
                 ll' = 0 : overlay (map (+sep) ll) (map (subtract sep) rl)
                 rr' = 0 : overlay (map (+sep) rr) (map (subtract sep) lr)

overlay :: [a] -> [a] -> [a]
overlay [] ys = ys
overlay xs [] = xs
overlay (x:xs) (y:ys) = x : overlay xs ys

{-
Problem 67
A string representation of binary trees

Somebody represents binary trees as strings of the following type:

a(b(d,e),c(,f(g,)))

a) Write a Prolog predicate which generates this string representation, if the tree is given as usual (as nil or t(X,L,R) term). Then write a predicate which does this inverse; i.e. given the string representation, construct the tree in the usual form. Finally, combine the two predicates in a single predicate tree_string/2 which can be used in both directions.
-}
tree_to_string :: (Show a) => BinaryTree a -> String
tree_to_string Empty = ""
tree_to_string (Branch a left right) = (show a) ++ "(" ++ tree_to_string left ++ "," ++ tree_to_string right ++ ")"

-- Again, from Haskell99 site. They are smart.
string_to_tree :: (Monad m) => String -> m (BinaryTree Char)
string_to_tree "" = return Empty
string_to_tree [x] = return (Branch x Empty Empty)
string_to_tree str = tfs str >>= \("",t) -> return t
               where tfs a@(x:xs) | x == ',' || x == ')' = return (a,Empty)
                     tfs (x:y:xs)
                         | y == ',' || y == ')' = return (y:xs, Branch x Empty Empty)
                         | y == '(' = do (',':xs', l) <- tfs xs
                                         (')':xs'', r) <- tfs xs'
                                         return $ (xs'', Branch x l r)
                     tfs _ = fail "bad parse"