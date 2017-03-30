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
-- Thanks Haskell99 and thanks Array.
-}

type WeightedGraph n w = Array n [(n,w)]
weighted1 = [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]

mk_weighted_graph :: (Ix i) => Bool -> (i,i) -> [(i,i,t)] -> (WeightedGraph i t)
mk_weighted_graph dir bnds es = accumArray
                  (\xs x -> x:xs)
                  []
                  bnds
                  ([(x1,(x2,w)) | (x1,x2,w) <- es] ++ if dir then [] else [(x2,(x1,w)) | (x1,x2,w) <- es, x1/=x2]) 
adjacent :: (Ix i) => WeightedGraph i t -> i -> [i]
adjacent g v = map fst (g!v)
 
nodes :: (Ix i) => WeightedGraph i t -> [i]
nodes g = indices g

edge_in :: (Ix i) => WeightedGraph i t -> (i,i) -> Bool
edge_in g (x,y) = elem y (adjacent g x)

weight :: (Ix i) => i -> i -> WeightedGraph i t -> t
weight x y g = head [c | (a,c) <- g!x, a == y]

edges_d :: (Ix i) => WeightedGraph i t -> [(i,i,t)]
edges_d g = [(v1,v2,w) | v1 <- nodes g, (v2,w) <- g!v1]

edges_u :: (Ix i) => WeightedGraph i t -> [(i,i,t)]
edges_u g = [(v1,v2,w) | v1 <- nodes g, (v2,w) <- g!v1, v1 < v2]

prim :: (Ix i, Ord t) => WeightedGraph i t -> [(t,i,i)]
prim g = prim' [n] ns []
     where (n:ns)         = nodes g
           es             = edges_u g
           prim' t [] mst = mst
           prim' t r mst  = let e@(c,u',v') = minimum [(c,u,v) | (u,v,c) <- es, elem u t, elem v r]
                            in prim' (v':t) (delete v' r) (e:mst)

{-
Problem 85

Determine if two graphs are isomorphic.
Many thanks Haskell99 for another brilliant solution.
-}

graphG1 = Graph [1, 2, 3, 4, 5, 6, 7, 8]
                    [(1, 5), (1, 6), (1, 7), (2, 5), (2, 6), (2, 8),
                    (3, 5), (3, 7), (3, 8), (4, 6), (4, 7), (4, 8)]
 
graphH1 = Graph [1, 2, 3, 4, 5, 6, 7, 8]
                    [(1, 2), (1, 4), (1, 5), (6, 2), (6, 5), (6, 7),
                    (8, 4), (8, 5), (8, 7), (3, 2), (3, 4), (3, 7)]
 
canonical :: (Ord a, Enum a) => Graph a -> String
canonical g = minimum $ map f (perm (length a))
            where
                Adjacency a = graph_to_adj g
                v = map fst a
                perm n = foldr (\x xs -> [i : s | i <- [1..n], s <- xs, notElem i s]) [[]] [1..n]
                f p = let n = zip v p
                      in  show [(snd x, sort id $ map (\x -> snd $ head $ snd $ break ((==) x . fst) n) $ snd $ find a x) | x <- sort snd n]
                sort f n = foldr (\x xs -> let (lt,gt) = break ((<) (f x) . f) xs in lt ++ [x] ++ gt) [] n
                find a x = let (xs,ys) = break ((==) (fst x) . fst) a in head ys

isomorphic :: (Ord a, Enum a, Ord b, Enum b) => Graph a -> Graph b -> Bool
isomorphic g@(Graph xs ys) h@(Graph xs' ys') = length xs   == length xs' &&
                                               length ys   == length ys' &&
                                               canonical g == canonical h

{-

Problem 86

a) Write a predicate degree(Graph,Node,Deg) that determines the degree of a given node.
b) Write a predicate that generates a list of all nodes of a graph sorted according to decreasing degree.
c) Use Welch-Powell's algorithm to paint the nodes of a graph in such a way that adjacent nodes have different colors.

Thanks again to Haskell99 for the wp_color implementation.
-}

-- Degree of a node.
node_degree :: (Eq a) => Graph a -> [(Int, a)]
node_degree g@(Graph xs ys) = map (\(v,vs) -> (length vs,v)) as
            where Adjacency as = graph_to_adj g

-- Array of nodes sorted in decreasing order.
nodes_decreasing :: (Eq a, Ord a) => Graph a -> [(Int, a)]
nodes_decreasing = sort . node_degree

-- Sorted Adjacency Graph. Everything is descending.
sort_graph :: (Eq a, Ord a) => Graph a -> Adjacency a
sort_graph g = Adjacency (map (\(a,b) -> (a, sort b 1 maximum)) (sort adj 1 maxv))
           where Adjacency adj = graph_to_adj g
                 sort [] _ _   = []
                 sort xs n f   = let m = f xs
                                 in  m : sort [x | x <- xs, x /= m] (n+1) f
                 maxv (x:xs)   = foldr(\a@(a1,_) b@(b1,_) -> if a1 > b1 then a else b) x xs

-- Welch-Powell algorithm to paint nodes such that adjacent nodes have different colors.
wp_color :: (Eq a, Ord a) => Graph a -> [(a, Int)]
wp_color g = color adj [] 1
         where
                Adjacency adj = sort_graph g
                color [] ys _ = ys
                color xs ys n = let ys' = color' xs ys n
                                in  color [x | x <- xs, notElem (fst x,n) ys'] ys' (n+1)
                color' [] ys n         = ys
                color' ((v,e):xs) ys n = if any (\x -> elem (x,n) ys) e
                                         then color' xs ys n
                                         else color' xs ((v,n):ys) n

{-
Problem 87

Depth-first order graph traversal.

Write a predicate that generates a depth-first order graph traversal sequence. The starting point should be specified, and the output should be a list of nodes that are reachable from this starting point (in depth-first order).
Clean solution from Haskell99.
-}

depth_first :: (Show a, Eq a) => Graph a -> a -> [a]
depth_first (Graph vs es) v
            | [x | x <- vs, x == v] == [] = []
            | otherwise                  = df_recursive (Graph vs es) [v]

df_recursive :: (Show a, Eq a) => Graph a -> [a] -> [a]
df_recursive (Graph [] _) _ = []
df_recursive (Graph _ _) [] = []
df_recursive (Graph vs es) (t:ts)
             | [x | x <- vs, x == t] == [] = df_recursive (Graph vs' es) ts
             | otherwise = t : df_recursive (Graph vs' es) (adjacent ++ ts)
             where
                adjacent = [x | (x,y) <- es, y == t] ++ [x | (y,x) <- es, y == t]
                vs' = [x | x <- vs, x /= t]


{-
Problem 88

Connected components 

Write a predicate that splits a graph into its connected components.
More help from Haskell99.
-}

connected :: (Show a, Eq a) => Graph a -> [[a]]
connected (Graph [] _) = []
connected g@(Graph (v:vs) es) 
          | remaining == [] = [c]
          | otherwise       = c : connected (Graph remaining es)
          where
                remaining = (v:vs) \\ c 
                c         = depth_first (Graph (v:vs) es) v

{-
Problem 89

Bipartite graphs

Write a predicate that finds out whether a given graph is bipartite.
Haskell99 to the rescue.
-}

bipartite :: (Eq a, Show a) => Graph a -> Bool
bipartite (Graph [] _) = True
bipartite g@(Graph (v:vs) es) = _bipartite g [(v,0)] [] []

_bipartite :: (Eq a, Show a) => Graph a -> [(a,Int)] -> [a] -> [a] -> Bool
_bipartite (Graph [] _) _ _ _ = True
_bipartite (Graph _ _) [] _ _ = True
_bipartite (Graph vs es) ((nv,0):stack) odd even
           | [x | x <- vs, x == nv] == []  = _bipartite (Graph vs es) stack odd even
           | [] == intersect adjacent even = _bipartite (Graph newv es) ([(x,1) | x <- adjacent] ++ stack) odd (nv : even)
           | otherwise                     = False
           where
                adjacent = [x | (x,y) <- es, y == nv] ++ [x | (y,x) <- es, y == nv]
                newv     = [x | x <- vs, x /= nv]
_bipartite (Graph vs es) ((nv,1):stack) odd even
           | [x | x <- vs, x == nv] == []  = _bipartite (Graph vs es) stack odd even
           | [] == intersect adjacent odd  = _bipartite (Graph newv es) ([(x,0) | x <- adjacent] ++ stack) (nv:odd) even
           | otherwise                     = False
           where
                adjacent = [x | (x,y) <- es, y == nv] ++ [x | (y,x) <- es, y == nv]
                newv     = [x | x <- vs, x /= nv]