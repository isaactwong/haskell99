import Data.List

{-
Generate K-regular simple graphs with N nodes

In a K-regular graph all nodes have a degree of K; i.e. the number of edges incident in each node is K. How many (non-isomorphic!) 3-regular graphs with 6 nodes are there?
Many many thanks to Haskell99 again.
-}

data Graph a = Graph [a] [(a,a)] deriving (Show, Eq)
data Adjacency a = Adjacency [(a,[a])] deriving (Show, Eq)

kSimpleGraphs :: Int -> Int -> [(Graph Int)]
kSimpleGraphs n k
  | n <= k = []
  | n < 0 = []
  | k < 0 = []
  | otherwise = map (adj_to_graph . fst) $
                     foldr (\x xs -> if any ((==) (snd x) . snd) xs then xs else (x:xs)) [] $
                     zip a $ map canonical a
  where
    a = filter (\(Adjacency a) -> all ((==) k .length . snd) a) $ map (graph_to_adj . Graph [1..n]) $ perm e q
    e = map (\xs -> (head xs, last xs)) $ perm [1..n] 2
    (q, r) = quotRem (n * k) 2
    perm n k = foldr (\x xs -> [(i:s) | i <- n, s <- xs, notElem i s, asc (i:s)]) [[]] [1..k]
    asc xs = all (uncurry (<)) $ zip xs $ tail xs

canonical :: (Eq a, Ord a) => Adjacency a -> String
canonical (Adjacency g) = minimum $ map f (permutations [1..n])
  where n = length g
        v = map fst g
        f p = let n = zip v p
              in show [(snd x,
                        sort id $ map (\x -> snd $ head $ snd $ break ((==) x . fst) n) $ snd $ find g x) |
                        x <- sort snd n]
        sort f n = foldr (\x xs -> let (lt, gt) = break ((<) (f x) . f) xs in lt ++ [x] ++ gt) [] n
        find a x = let (xs, ys) = break ((==) (fst x) . fst) a in head ys

-- Adjacency to Graph
adj_to_graph :: (Eq a) => Adjacency a -> Graph a
adj_to_graph (Adjacency []) = Graph [] []
adj_to_graph (Adjacency xs) = Graph (nodes xs) (dups . edges $ xs)
             where nodes [] = []
                   nodes ((v,_)  : adjs)  = v : (nodes adjs)
                   edges [] = []
                   edges ((v,es) : adjs)  = (map ((,) v) es) ++ edges adjs
                   dups [] = []
                   dups ((a,b) : as) = (a,b) : (dups (filter (\(x,y) -> (x,y) /= (a,b) && (x,y) /= (b,a)) as))

-- Graph to Adjacency
graph_to_adj :: (Eq a) => Graph a -> Adjacency a
graph_to_adj (Graph vs es) = Adjacency (foldl (\adj v -> (v, f v es) : adj) [] vs)
             where f x xs = map (\(e1,e2) -> if e1 == x then e2 else e1) (filter (\(e1,e2) -> e1==x || e2==x) xs)
