-- Problem 54A: Check whether a given term represents a binary tree
-- Done! Haskell's type system ensures this for us! Yeah Haskell!

-- Problem 55: Construct completely balanced binary trees.
-- Binary trees are defined to be balanced if |#-nodes-left-subtree - #-nodes-right-subtree| <= 1
-- More help from the website. This one is magic.
data BinaryTree a = Empty |  Branch a (BinaryTree a) (BinaryTree a) deriving Show

balancedBinaryTrees :: Int -> [BinaryTree Char]
balancedBinaryTrees 0 = [Empty]
balancedBinaryTrees n = let (q, r) = quotRem (n-1) 2
                    in [Branch 'x' left right | i <- [q..(q+r)],
                       left <- balancedBinaryTrees i,
                       right <- balancedBinaryTrees (n-i-1)]