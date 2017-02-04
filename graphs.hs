{-
Problem 80: Conversions

Write predicates to convert between the different graph representations. With these predicates, all representations are equivalent; i.e. for the following problems you can always pick freely the most convenient form. The reason this problem is rated (***) is not because it's particularly difficult, but because it's a lot of work to deal with all the special cases.
-}

data Graph a     = Graph [a] [(a,a)]   deriving (Show, Eq)
data Adjacency a = Adjacency [(a,[a])] deriving (Show, Eq)
data Friendly a  = Friendly [(a,a)]    deriving (Show, Eq)

graph1 = Graph ['g','h','b','c','f','k','d'] [('g','h'),('b','c'),('b','f'),('f','c'),('k','f')]

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
