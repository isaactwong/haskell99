import Data.List
import Data.Array

{-
Problem 80: Conversions

Write predicates to convert between the different graph representations. With these predicates, all representations are equivalent; i.e. for the following problems you can always pick freely the most convenient form. The reason this problem is rated (***) is not because it's particularly difficult, but because it's a lot of work to deal with all the special cases.
-}

data Graph a     = Graph     [a] [(a,a)]   deriving (Show, Eq)
data Adjacency a = Adjacency [(a,[a])]     deriving (Show, Eq)
data Friendly a  = Friendly  [(a,a)]       deriving (Show, Eq)

graph1    = Graph ['g','h','b','c','f','k','d'] [('g','h'),('b','c'),('b','f'),('f','c'),('k','f')]
graph2    = Graph ['a', 'b', 'c', 'd'] [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')]
adj1      = Adjacency [('b', ['c','f']),('c',['b','f']),('f',['b','c']),('k', ['f'])]
friendly1 = Friendly [('b','c'),('f','c'),('g','h'),('d','d'),('f','b'),('k','f'),('h','g')]

-- Graph to Adjacency
graph_to_adj :: (Eq a) => Graph a -> Adjacency a
graph_to_adj (Graph vs es) = Adjacency (foldl (\adj v -> (v, f v es) : adj) [] vs)
             where f x xs = map (\(e1,e2) -> if e1 == x then e2 else e1) (filter (\(e1,e2) -> e1==x || e2==x) xs)

-- Cool one from Haskell99. Remember we can define implicitly through equations. So we define zs by the implicit equation Adjacency zs = graphToAdj (Graph vs es). Very neat!
graphToAdj :: (Eq a) => Graph a -> Adjacency a
graphToAdj (Graph [] _)      = Adjacency []
graphToAdj (Graph (v:vs) es) = Adjacency ((v, es >>= (f v)) : zs)
           where f v1 (e1,e2)
                   | v1 == e2  = [e1]
                   | v1 == e1  = [e2]
                   | otherwise = []
                 Adjacency zs = graphToAdj (Graph vs es)

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

-- Cool one from Haskell99
adjToGraph :: (Eq a) => Adjacency a -> Graph a
adjToGraph (Adjacency [])        = Graph [] []
adjToGraph(Adjacency ((v,a):vs)) = Graph (v:xs) ((a>>=(f v)) ++ ys)
                     where f y x = if elem (y,x) ys || elem (x,y) ys
                                   then []
                                   else [(y,x)]
                           Graph xs ys = adjToGraph (Adjacency vs)

-- Graph to Friendly
-- Thanks to Haskell99 for the all function.
graph_to_friendly :: (Eq a) => Graph a -> Friendly a
graph_to_friendly (Graph vs es) = Friendly (es ++ zip g g)
                  where g = filter (\x -> (all (\(a,b) -> x/=a && x/=b) es)) vs

-- Friendly to Graph
-- Thanks to Haskell99 for the cool uncurry function.
friendly_to_graph :: (Eq a) => Friendly a -> Graph a
friendly_to_graph (Friendly fs) = Graph (nub vs) es
                  where vs = foldr (\(x,y) acc -> [x,y] ++ acc) [] fs
                        es = filter (uncurry (/=)) fs
                        
-- Adjacency to Friendly
adj_to_friendly :: (Eq a) => Adjacency a -> Friendly a
adj_to_friendly = graph_to_friendly . adj_to_graph

-- Friendly to Adjacency
friendly_to_adj :: (Eq a) => Friendly a -> Adjacency a
friendly_to_adj = graph_to_adj . friendly_to_graph 

{-
Problem 81
Path from one node to another one.
Write a function that, given two nodes a and b in a graph, returns all the acyclic paths from a to b.
Solution is inspired from Haskell99.
-}

paths :: (Eq a, Show a) => a -> a -> Friendly a -> [[a]]
paths x y (Friendly es)
      | x == y    = [[x]]
      | otherwise = first_step x y es []

first_step :: (Eq a, Show a) => a -> a -> [(a,a)] -> [a] -> [[a]]
first_step x y edges path = next_step x y edges path ([y1 | (x1,y1) <- edges, x1==x] ++ [x1 | (x1,y1) <- edges, y1==x])

next_step :: (Eq a, Show a) => a -> a -> [(a,a)] -> [a] -> [a] -> [[a]]
next_step x y edges path []
          | x == y    = [path++[y]]
          | otherwise = []
next_step x y edges path (z:zs)
          | x == y      = [path++[y]]
          | elem x path = []
          | otherwise   = (first_step z y edges (path++[x])) ++ (next_step x y edges path zs)

{-
Problem 82

Cycle from a given node

Write a predicate cycle(G,A,P) to find a closed path (cycle) P starting at a given node A in the graph G. The predicate should return all cycles via backtracking.
-}
-- From the smart people at Haskell99
cycle' :: (Eq a, Show a) => a -> Graph a -> [[a]]
cycle' a (Graph ns xs) = cycle'' a xs

cycle'' :: (Eq a, Show a) => a -> [(a,a)] -> [[a]]
cycle'' a xs = [(a:p) | e <- xs, (fst e == a), p <- paths'' (snd e) a [x | x <- xs, x /= e]]
               ++
               [(a:p) | e <- xs, (snd e) == a, p <- paths'' (fst e) a [x | x <- xs, x/= e]]

{-
Problem 83

Construct all spanning trees

Write a predicate s_tree(Graph,Tree) to construct (by backtracking) all spanning trees of a given graph. With this predicate, find out how many spanning trees there are for the graph depicted to the left. The data of this example graph can be found in the file p83.dat. When you have a correct solution for the s_tree/2 predicate, use it to define two other useful predicates: is_tree(Graph) and is_connected(Graph). Both are five-minutes tasks!
-}

-- From Haskell99, and I think clearer and cooler than the earlier paths solutions.
paths' :: (Eq a, Show a) => a -> a -> Graph a -> [[a]]
paths' a b (Graph ns xs) = paths'' a b xs

paths'' :: (Eq a, Show a) => a -> a -> [(a,a)] -> [[a]]
paths'' a b xs
        | a == b    = [[a]]
        | otherwise = concat [map (a:) (paths'' d b [x | x <- xs, x /= (c,d)]) | (c,d) <- xs, c==a]
                      ++
                      concat [map (a:) (paths'' c b [x | x <- xs, x /= (c,d)]) | (c,d) <- xs, d==a]

-- Thanks again Haskell99.
spanning_tree :: (Eq a, Show a) => Graph a -> [Graph a]
spanning_tree (Graph xs ys) = filter connected $ filter (not . cycles) $ filter nodes allTrees
              where
                allTrees                       = [Graph (extract_nodes edges) edges | edges <- foldr acc [[]] ys]
                acc e es                       = es ++ (map (e:) es)
                extract_nodes e                = nub $ concat $ map (\(a,b) -> [a,b]) e
                nodes (Graph xs' _)            = length xs == length xs'
                cycles (Graph xs' ys')         = any((/=) 0 . length . flip cycle'' ys') xs'
                connected (Graph (x':xs') ys') = not $ any (null) [paths'' x' y' ys' | y' <- xs']

{-
Problem 84

Construct the minimal spanning tree

-}

type WeightedGraph n w = Array n [(n,w)]
weighted1 = [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]

makeWeightedGraph :: (Ix i) => Bool -> (i,i) -> [(i,i,t)] -> (WeightedGraph i t)
makeWeightedGraph dir bnds es = accumArray
                  (\xs x -> x:xs)
                  []
                  bnds
                  ([(x1,(x2,w)) | (x1,x2,w) <- es] ++ if dir then [] else [(x2,(x1,w)) | (x1,x2,w) <- es, x1/=x2]) 





