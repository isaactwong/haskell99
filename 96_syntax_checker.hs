{-
Syntax checker
In a certain programming language (Ada) identifiers are defined by the syntax diagram below. <see haskell99>
Transform the syntax diagram into a system of syntax diagrams which do not contain loops; i.e. which are purely recursive. Using these modified diagrams, write a predicate identifier/1 that can check whether or not a given string is a legal identifier.
-}

import Data.Char

isValidIdentifier :: String -> Bool
isValidIdentifier [] = False
isValidIdentifier (x:xs) = (isLetter x) && (parse xs)
  where parse [] = True
        parse (y:ys)
          | y == '-'     = (length ys > 0) && (isNumber (head ys)) && parse ys
          | isAlphaNum y = parse ys
          | otherwise    = False


