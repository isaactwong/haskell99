{-
English number words: 
On financial documents, like cheques, numbers must sometimes be written in full words. Example: 175 must be written as one-seven-five. Write a predicate full-words/1 to print (non-negative) integer numbers in full words.
Thanks to Haskell99 for intersperse!
-}

import Data.Char
import Data.List

digitsToWords :: Int -> String
digitsToWords n = concat $ intersperse "-" [words !! (digitToInt i) | i <- (show n)]
  where words = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]


