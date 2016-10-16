import Control.Monad (replicateM)

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