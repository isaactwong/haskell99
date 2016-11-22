-- Problem 54A: Check whether a given term represents a binary tree
-- Done! Haskell's type system ensures this for us! Yeah Haskell!

-- Problem 55: Construct completely balanced binary trees.
-- Binary trees are defined to be balanced if |#-nodes-left-subtree - #-nodes-right-subtree| <= 1
data BinaryTree a = Leaf a | (Branch a (BinaryTree a) (BinaryTree a) 

balancedBinaryTrees :: Int -> [BinaryTree Char]
