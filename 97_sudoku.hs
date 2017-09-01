{-# LANGUAGE FlexibleContexts #-}
import System.Environment
import Control.Monad
import Data.List
import Data.Array.IO

{-
Sudoku puzzles go like this:

       Problem statement                 Solution

        .  .  4 | 8  .  . | .  1  7	     9  3  4 | 8  2  5 | 6  1  7
                |         |                          |         |
        6  7  . | 9  .  . | .  .  .	     6  7  2 | 9  1  4 | 8  5  3
                |         |                          |         |
        5  .  8 | .  3  . | .  .  4          5  1  8 | 6  3  7 | 9  2  4
        --------+---------+--------          --------+---------+--------
        3  .  . | 7  4  . | 1  .  .          3  2  5 | 7  4  8 | 1  6  9
                |         |                          |         |
        .  6  9 | .  .  . | 7  8  .          4  6  9 | 1  5  3 | 7  8  2
                |         |                          |         |
        .  .  1 | .  6  9 | .  .  5          7  8  1 | 2  6  9 | 4  3  5
        --------+---------+--------          --------+---------+--------
        1  .  . | .  8  . | 3  .  6	     1  9  7 | 5  8  2 | 3  4  6
                |         |                          |         |
        .  .  . | .  .  6 | .  9  1	     8  5  3 | 4  7  6 | 2  9  1
                |         |                          |         |
        2  4  . | .  .  1 | 5  .  .          2  4  6 | 3  9  1 | 5  7  8

Every spot in the puzzle belongs to a (horizontal) row and a (vertical) column, as well as to one single 3x3 square (which we call "square" for short). At the beginning, some of the spots carry a single-digit number between 1 and 9. The problem is to fill the missing spots with digits in such a way that every number between 1 and 9 appears exactly once in each row, in each column, and in each square.

Holy smokes. Thanks Haskell99.
-}


type SudokuBoard = IOArray Int Int

main  = do
  [f] <- getArgs
  a <- newArray (1, 81) 0
  readFile f >>= readSudokuBoard a
  putStrLn "Original:"
  printSudokuBoard a
  putStrLn "Solutions:"
  solve a (1,1)

readSudokuBoard :: (MArray a e m, Ix i, Read e, Num i, Enum i) => a i e -> String -> m ()
readSudokuBoard a xs = sequence_ $ do (i, ys) <- zip [1..9] (lines xs)
                                      (j,n)   <- zip [1..9] (lines ys)
                                      return $ writeBoard a (j, i) (read n)

printSudokuBoard :: (MArray a a1 IO, Ix i, Show a1, Num i, Enum i) => a i a1 -> IO ()
printSudokuBoard a = do putStrLn "-----------"
                        mapM_ (\y -> putStr "|" >> printLine a y >> putStrLn "|") [1..9]
                        putStrLn "-----------"
  where
    printLine a y = mapM (\x -> readBoard a (x, y)) [1..9] >>= mapM_ (putStr . show)

solve :: SudokuBoard -> (Int, Int) -> IO (Maybe SudokuBoard)
solve a (10, y) = solve a (1, y+1)
solve a (_, 10) = printSudokuBoard a >> return (Just a)
solve a (x, y)  = do v <- readBoard a (x, y)
                     case v of
                       0 -> availableNums a (x, y) >>= solve' a (x, y)
                       _ -> solve a (x, y)
  where solve' a (x, y) [] = return Nothing
        solve' a (x, y) (v:vs) = do writeBoard a (x, y) v
                                    r <- solve a (x+1, y)
                                    writeBoard a (x, y) 0
                                    solve' a (x, y) vs

getRowNums :: (MArray a1 a m, Ix i, Num i, Enum i) => a1 i a -> i -> m [a]
getRowNums a y = sequence [readBoard a (x', y) | x' <- [1..9]]

getColNums :: (MArray a1 a m, Ix i, Num i, Enum i) => a1 i a -> i -> m [a]
getColNums a x = sequence [readBoard a (x, y') | y' <- [1..9]]

getBoxNums :: (MArray a1 a m, Ix a2, Integral a2) => a1 a2 a -> (a2, a2) -> m [a]
getBoxNums a (x, y) = sequence [readBoard a (x'+u, y'+u) | u <- [0..2], v<- [0..2]]
  where x' = (3 * (quot (x-1) 3)) + 1
        y' = (3 * (quot (y-1) 3)) + 1

availableNums :: (MArray a1 a m, Ix a2, Num a, Integral a2, Eq a, Enum a) => a1 a2 a -> (a2, a2) -> m [a]
availableNums a (x, y) = do r <- getRowNums a y
                            c <- getColNums a x
                            b <- getBoxNums a (x, y)
                            return $ [0..9] \\ (union r (union c b))

readBoard a (x,y) = readArray a (x + 9*(y-1))
writeBoard a (x,y) e = writeArray a (x + 9*(y-1)) e
