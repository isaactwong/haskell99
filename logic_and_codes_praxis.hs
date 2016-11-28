import Control.Monad (replicateM)
import Data.List
import Data.Ord (comparing)

-- Problem 46: Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.
-- Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.

not' :: Bool -> Bool
not' True = False
not' False = True

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True


nand' :: Bool -> Bool -> Bool
nand' a b = not' $ and' a b

nor' :: Bool -> Bool -> Bool
nor' a b = not' (or' a b)

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

-- A implies B, otherwise False.
impl' :: Bool -> Bool -> Bool
impl' True True = True
impl' False False = True
impl' True False = False
impl' False True = True

equ' :: Bool -> Bool -> Bool
equ' True True = True
equ' False False = True
equ' _ _ = False

-- Print out the table for all combinations of True and False
logicTable :: (Bool -> Bool -> Bool) -> IO ()
logicTable f = do
           putStrLn $ "True | True | " ++ show (f True True)
           putStrLn $ "True | False | " ++ show (f True False)
           putStrLn $ "False | True | " ++ show (f False True)
           putStrLn $ "False | False | " ++ show (f False False)

-- Another cool one from Haskell99
-- logicTable f =  apM_ putStrLn [(show a) ++ " | " ++ (show b) ++ " | " ++ (show (f a b)) | a <- [True, False], b <- [True, False]]

-- Problem 47: Continue problem P46 by defining and/2, or/2, etc as being operators.
-- No idea what the Java ones are so stole the values from Haskell99.
infixl 4 `or'`
infixl 6 `and'`

-- Problem 48: Generalize problem 47 in such a way that the logical expression may contain any number of logical variables. Again, the geniuses at Haskell99 provided this solution.
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn [toStr a ++ " => " ++ show (f a) | a <- args n]
    where args n = replicateM n [True, False]
          toStr = unwords . map (\x -> show x ++ space x)
          space True = "  "
          space False = " "

-- Problem 49: Gray codes. An n-bit Gray code is a sequence of n-bit storage strings according to certain rules.
-- n = 1: C(1) = ['0','1'].
-- n = 2: C(2) = ['00','01','11','10'].
-- n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
-- Find out the construction rules and write a predicate with the following specification:
-- Gray codes are codes where each successive word differs from those around in only one position. So we can't just do replicateM n [0,1].
gray :: Int -> [String]
gray 0 = [""]
gray n = ["0" ++ x | x <- gray (n-1)] ++ ["1" ++ x | x <- (reverse . gray) (n-1)]

-- Problem 50: Huffman Codes.
-- [("a",45),("b",13),("c",12),("d",16),("e",9),("f",5)]
data HuffmanTree a = Leaf a | Branch (HuffmanTree a) (HuffmanTree a) deriving Show
huffmanEncoding :: (Ord c, Ord f, Num f) => [(c, f)] -> [(c, [Char])]
huffmanEncoding source = sortBy (comparing fst) $ serialize $ buildHTree $ addLeafs $ sortByFreqs source
                where
                        sortByFreqs xs = sortBy (comparing snd) xs
                        addLeafs xs = map (\(c, f) -> (Leaf c, f)) xs
                        buildHTree [(htree, _)] = htree
                        buildHTree ((htree1, freq1) : (htree2, freq2) : hfs) =
                                   buildHTree $ insertBy (comparing snd) (Branch htree1 htree2, freq1 + freq2) hfs
                        serialize (Branch l r) = 
                                 [(x, '0' : code) | (x, code) <- serialize l] ++
                                 [(x, '1' : code) | (x, code) <- serialize r]
                        serialize (Leaf x) = [(x, "")]