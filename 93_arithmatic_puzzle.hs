import Control.Monad
import Data.Maybe
import Data.List

{-
Given a list of integer numbers, find a correct way of inserting arithmetic signs (operators) such that the result is a correct equation. Example: With the list of numbers [2,3,5,7,11] we can form the equations 2-3+5+7 = 11 or 2 = (3*5+7)/11 (and ten others!).

Division should be interpreted as operating on rationals, and division by zero should be avoided.

Thank you Haskell99 for more cool solutions.
-}

type Equation = (Expression, Expression)
data Expression = Const Integer | Binary Expression Op Expression deriving (Show, Eq)
data Op = Plus | Minus | Multiply | Divide deriving (Bounded, Eq, Enum, Show)
type Value = Rational


validEquations :: [Integer] -> [String]
validEquations ns = map (flip showsEquation "") (equations ns)

-- Generate all the valid equations from a list of integers.
equations :: [Integer] -> [Equation]
equations [] = error "Empty List"
equations [n] = error "Only one number"
equations ns = [(e1, e2) | (ns1, ns2) <- splits ns,
                           (e1, v1) <- expressions ns1,
                           (e2, v2) <- expressions ns2,
                           v1 == v2]

expressions :: [Integer] -> [(Expression, Value)]
expressions [n] = [(Const n, fromInteger n)]
expressions ns = [(Binary e1 op e2, v) | (ns1, ns2) <- splits ns,
                                         (e1, v1) <- expressions ns1,
                                         (e2, v2) <- expressions ns2,
                                         op <- [minBound..maxBound],
                                         not (rightAssociative op e2),
                                         v <- maybeToList (apply op v1 v2)]

-- Split an array into all possible inital sub-arrays followed by the remaining subarray. 
splits :: [a] -> [([a], [a])]
splits xs = tail (init (zip (inits xs) (tails xs)))

apply :: Op -> Value -> Value -> Maybe Value
apply Plus x y = Just (x+y)
apply Minus x y = Just (x-y)
apply Multiply x y = Just (x*y)
apply Divide x 0 = Nothing
apply Divide x y = Just (x/y)

rightAssociative :: Op -> Expression -> Bool
rightAssociative Plus (Binary _ Plus _) = True
rightAssociative Plus (Binary _ Minus _) = True
rightAssociative Multiply (Binary _ Multiply _) = True
rightAssociative Multiply (Binary _ Divide _) = True
rightAssociative _ _ = False

showsEquation :: Equation -> ShowS
showsEquation (l,r) = showsExprPrec 0 l . showString " = " . showsExprPrec 0 r

showsExprPrec :: Int -> Expression -> ShowS
showsExprPrec _ (Const n) = shows n
showsExprPrec p (Binary e1 op e2) =
  showParen (p > op_prec) $ showsExprPrec op_prec e1 . showString (opName op) . showsExprPrec op_prec e2
  where op_prec = precedence op

precedence :: Op -> Int
precedence Plus = 6
precedence Minus = 6
precedence Multiply = 7
precedence Divide = 7
 
opName :: Op -> String
opName Plus = "+"
opName Minus = "-"
opName Multiply = "*"
opName Divide = "/"
