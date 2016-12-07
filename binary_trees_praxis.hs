-- Problem 54A: Check whether a given term represents a binary tree
-- Done! Haskell's type system ensures this for us! Yeah Haskell!

-- Definition of a binary tree.
data BinaryTree a = Empty |  Branch a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

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

countNodes :: BinaryTree a -> Int
countNodes Empty = 0
countNodes (Branch x left right) = 1 + (countNodes left) + (countNodes right)

