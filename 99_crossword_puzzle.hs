import Data.List
import Data.Function (on)
import Data.Ord (comparing)
import Control.Monad (guard)
{-
Given an empty (or almost empty) framework of a crossword puzzle and a set of words. The problem is to place the words into the framework.

The particular crossword puzzle is specified in a text file which first lists the words (one word per line) in an arbitrary order. Then, after an empty line, the crossword framework is defined. In this framework specification, an empty character location is represented by a dot (.). In order to make the solution easier, character locations can also contain predefined character values. The puzzle above is defined in the file p7_09a.dat, other examples are p7_09b.dat and p7_09d.dat. There is also an example of a puzzle (p7_09c.dat) which does not have a solution.

Words are strings (character lists) of at least two characters. A horizontal or vertical sequence of character places in the crossword puzzle framework is called a site. Our problem is to find a compatible way of placing words onto sites.

Hints: (1) The problem is not easy. You will need some time to thoroughly understand it. So, don't give up too early! And remember that the objective is a clean solution, not just a quick-and-dirty hack!

(2) Reading the data file is a tricky problem for which a solution is provided in the file p7_09-readfile.pl. See the predicate read_lines/2.

(3) For efficiency reasons it is important, at least for larger puzzles, to sort the words and the sites in a particular order. For this part of the problem, the solution of P28 may be very helpful.
-}

type Coord = (Int, Int)
type Wort = String
data Site = Site { siteCoords :: [Coord], siteLen :: Int } deriving (Show, Eq)
data Crossword = Crossword { cwWords :: [Wort], cwSites :: [Site] } deriving (Show, Eq)

-- equalling = ((==) `on`)

toSites :: [String] -> [Site]
toSites lines = find (index_it lines) ++ find (transpose .index_it $ lines)
  where find = map makePos . concatMap extractor
        extractor = filter ((>1) . length) . map (filter ((=='.') . snd)) . groupBy (equaling snd)
        index_it = zipWith (\row -> zip [(col, row) | col <- [1..]]) [1..]
        makePos xs = Site { siteCoords = map fst xs, siteLen = length xs }
{-
noCollisions :: [(String, Site)] -> Bool
noCollisions xs = all allEqual groupedByCoord
  where groupedByCoord map (map snd) . groupBy (equaling fst) . sortBy (comparing fst) . concatMap together $ xs
  o
-}

noCollision :: [(String, Site)] -> Bool
noCollision xs = all allEqual groupedByCoord
    where groupedByCoord = map (map snd) . groupBy (equaling fst) . sortBy (comparing fst) . concatMap together $ xs
          allEqual []     = True
          allEqual (x:xs) = all (x==) xs

together :: (Word, Site) -> [(Coord, Char)]
together (w, s) = zip (siteCoords s) w

solve :: Crossword -> [[(Coord, Char)]]
solve cw = map (concatMap together) solution
  where solution = solve' (cwWords cw) (cwSites cw)

solve' :: [Wort] -> [Site] -> [[(Wort, Site)]]
solve' _ [] = [[]]
solve' words (s : ss) = if null possWords
                           then error ("too few words of length" ++ show (siteLen s))
                           else do try <- possWords
                                   let restWords = Data.List.delete try words
                                   more <- solve' restWords ss
                                   let attempt = (try, s) : more
                                   Control.Monad.guard $ noCollision attempt
                                   return attempt
  where possWords = filter (\w -> siteLen s == length w) words
