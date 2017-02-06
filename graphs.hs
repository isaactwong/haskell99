import Data.List

{-
Problem 80: Conversions

Write predicates to convert between the different graph representations. With these predicates, all representations are equivalent; i.e. for the following problems you can always pick freely the most convenient form. The reason this problem is rated (***) is not because it's particularly difficult, but because it's a lot of work to deal with all the special cases.
-}

data Graph a     = Graph [a] [(a,a)]   deriving (Show, Eq)
data Adjacency a = Adjacency [(a,[a])] deriving (Show, Eq)
data Friendly a  = Friendly [(a,a)]    deriving (Show, Eq)

graph1    = Graph ['g','h','b','c','f','k','d'] [('g','h'),('b','c'),('b','f'),('f','c'),('k','f')]
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
                        