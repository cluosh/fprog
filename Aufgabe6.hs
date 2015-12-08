module Aufgabe6 where

-----------
-- PART 2
-----------
data Num a => Tree a = Nil |
     Node (a -> a) (a -> Bool) [a] (Tree a) (Tree a) (Tree a)
     

-- Define show class for Tree
instance Num a => Show (Tree a) where
  show Nil = "Nil"
  show (Node _ _ l n1 n2 n3) = "Node " ++ (show l) ++ 
                               " (" ++ (show n1) ++ 
                               ") (" ++ (show n2) ++ 
                               ") (" ++ (show n3) ++ ")"                           

-- Define smt function on tree
smt :: Num a => (Tree a) -> (Tree a)
smt Nil = Nil
smt (Node f p xs n1 n2 n3) = (Node f p (map f (filter p xs)) 
                                   (smt n1) (smt n2) (smt n3))

-----------
-- PART 3
-----------
data Num a => STree a = SNil |
     SNode [a] (STree a) (STree a) (STree a) deriving (Eq,Show)

-- Convert a tree from part 2 to an STree
t2st :: Num a => (Tree a) -> (STree a)
t2st Nil                    = SNil
t2st (Node _ _ xs n1 n2 n3) = (SNode xs (t2st n1) (t2st n2) (t2st n3))

-- 'Fold' tree
tsum :: Num a => (STree a) -> a
tsum SNil                = 0
tsum (SNode xs n1 n2 n3) = (foldl (+) 0 xs) + (tsum n1) + (tsum n2) + 
                           (tsum n3)
                           
-- Calculate depth of a tree
tdepth :: Num a => (STree a) -> Integer
tdepth SNil               = 0
tdepth (SNode _ n1 n2 n3) = max (tdepth n1 + 1) (tdepth n2 + 1)

-----------
-- PART 4
-----------
type Node     = Integer
type Edge     = (Node,Node)
type Source   = Node
type Sink     = Node
newtype Graph = Gph [(Source,[Sink])] deriving (Eq,Show)

-- Quicksort for node or edge sorting
qs :: Ord a => [a] -> [a]
qs []     = []
qs (x:xs) = qs (filter (< x) xs) ++ [x] ++ qs (filter (>= x) xs)

-- Remove duplicates from a list, with the list being sorted
rdup :: Eq a => a -> [a] -> [a]
rdup y (x:[])
  | y == x    = []
  | otherwise = [x]
rdup y (x:xs)
  | y == x    = rdup y xs
  | otherwise = [x] ++ (rdup x xs)

-- Get all nodes and edges of a graph
ne :: Graph -> ([Node],[Edge])
ne (Gph [])   = ([],[])
ne g = (n,e)
  where
    n = rdup (-1) (qs (nodes g))
    e = rdup (-1,-1) (qs (edges g))

-- Get all nodes of a graph, including duplicates
nodes :: Graph -> [Node]
nodes (Gph [])     = []
nodes (Gph (x:xs)) = (nodes (Gph xs)) ++ (snd x) ++ [fst x]

-- Get all edges of a graph, including duplicates
edges :: Graph -> [Edge]
edges (Gph [])     = []
edges (Gph (x:xs)) = (edges (Gph xs)) ++ 
                     [(min y z, max y z) | y <- [fst x], z <- (snd x)]
