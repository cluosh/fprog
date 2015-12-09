module Aufgabe6 where

-----------
-- PART 1
-----------

data Zeichen = A | B | C | D | E | F | G | H | I | J | K | L | M | N | 
               O | P | Q | R | S | T | U | V | W | X | Y | Z
               deriving (Eq,Ord,Show,Read,Enum,Bounded)
data Ziffer = Null | Eins | Zwei | Drei | Vier | Fuenf | Sechs | 
              Sieben | Acht | Neun 
              deriving (Eq,Ord,Show,Read,Enum,Bounded)
newtype Nat = Nat ((Zeichen,Zeichen,Zeichen),(Ziffer,Ziffer,Ziffer))

-- Convert Nat to Integer for faster calculation
integerFromNat :: Nat -> Integer
integerFromNat (Nat ((a,b,c),(d,e,f))) = x + y + z + u + v + w
  where
    x = toInteger (fromEnum f)
    y = 10 * (toInteger (fromEnum e))
    z = 100 * (toInteger (fromEnum d))
    u = 1000 * (toInteger (fromEnum c))
    v = 26000 * (toInteger (fromEnum b))
    w = 676000 * (toInteger (fromEnum a))

-- Convert Integer to Nat for faster calculation
natFromInteger :: Integer -> Nat
natFromInteger a
  | a < 0               = Nat ((A,A,A),(Null,Null,Null))
  | a < (toInteger max) = Nat ((b,c,d),(e,f,g))
  | otherwise           = max
  where
    x = mod a 10
    y = div (mod a 100 - x) 10
    z = div (mod a 1000 - y - x) 100
    u = div (mod a 26000 - z - y - x) 1000
    v = div (mod a 676000 - u - z - y - x) 26000
    w = div (a - v - u - z - y - x) 676000
    b = toEnum (fromInteger w)
    c = toEnum (fromInteger v)
    d = toEnum (fromInteger u)
    e = toEnum (fromInteger z) 
    f = toEnum (fromInteger y)
    g = toEnum (fromInteger x)
    max = Nat ((Z,Z,Z),(Neun,Neun,Neun))

-- Inequality
instance Eq Nat where
  Nat a /= Nat b = a /= b

-- Comparison
instance Ord Nat where
  compare (Nat a) (Nat b) = compare a b
    
-- Show
instance Show Nat where
  show (Nat ((a,b,c),(x,y,z))) = "\"" ++ show a ++ show b ++ show c ++ 
                                 " " ++ show (fromEnum x) ++ 
                                 show (fromEnum y) ++ 
                                 show (fromEnum z) ++ "\""

-- Implementing instance of Num for Nat
instance Num Nat where
  (+) a b = fromInteger (rem res (integerFromNat max + 1))
	where
	  res = integerFromNat a + 
	        integerFromNat b
	  max = Nat ((Z,Z,Z),(Neun,Neun,Neun))
  (*) a b = fromInteger (rem res (integerFromNat max + 1))
	where
	  res = integerFromNat a *
	        integerFromNat b
	  max = Nat ((Z,Z,Z),(Neun,Neun,Neun))
  (-) a b = fromInteger (rem res (integerFromNat max + 1))
    where
      res = integerFromNat a -
            integerFromNat b
      max = Nat ((Z,Z,Z),(Neun,Neun,Neun))
  signum a
	| a > zero  = 1
	| otherwise = 0
	where
	  zero      = fromInteger 0
  abs a         = a
  negate a      = fromInteger 0
  fromInteger a = natFromInteger a
  
-- Bounded
instance Bounded Nat where
  minBound      = Nat ((A,A,A),(Null,Null,Null))
  maxBound      = Nat ((Z,Z,Z),(Neun,Neun,Neun))

-- Implementing minimum enum instance for Nat
instance Enum Nat where
  toEnum a      = fromInteger (rem (toInteger a) 
                  (toInteger (maxBound :: Nat) + 1))
  fromEnum a    = fromIntegral (rem (toInteger a) 
                  (toInteger (maxBound :: Nat) + 1))
  
-- Implementing minimum real instance for Nat
instance Real Nat where
  toRational a  = toRational (toInteger a)

-- Implementing instance of Integral for Nat
instance Integral Nat where
  rem a b
    | a > b     = a - b
    | otherwise = b - a
  div a b
    | b > 0     = fromInteger (div (toInteger a) (toInteger b))
    | otherwise = error "divide by zero"
  quotRem a b   = (div a b, rem a b)
  toInteger a   = integerFromNat a

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
rdup1 :: Eq a => a -> [a] -> [a]
rdup1 _ []    = []
rdup1 y (x:[])
  | y == x    = []
  | otherwise = [x]
rdup1 y (x:xs)
  | y == x    = rdup1 y xs
  | otherwise = [x] ++ (rdup1 x xs)
  
-- Remove duplicates from a list with the list being sorted
rdup :: Eq a => [a] -> [a]
rdup []     = []
rdup (x:xs) = [x] ++ (rdup1 x xs)

-- Get all nodes and edges of a graph
ne :: Graph -> ([Node],[Edge])
ne (Gph [])   = ([],[])
ne g = (n,e)
  where
    n = rdup (qs (nodes g))
    e = rdup (qs (edges g))

-- Get all nodes of a graph, including duplicates
nodes :: Graph -> [Node]
nodes (Gph [])     = []
nodes (Gph (x:xs)) = (nodes (Gph xs)) ++ (snd x) ++ [fst x]

-- Get all edges of a graph, including duplicates
edges :: Graph -> [Edge]
edges (Gph [])     = []
edges (Gph (x:xs)) = (edges (Gph xs)) ++ 
                     [(min y z, max y z) | y <- [fst x], z <- (snd x)]

-- Create all edges from nodes
nedges :: [Node] -> [Edge]
nedges (x:[]) = [(x,x)]
nedges (x:xs) = [(x,y) | y <- xs] ++ nedges xs

-- Common possible edges with all edges
ecommon :: [Edge] -> [Edge] -> [Edge]
ecommon _ []     = []
ecommon [] _     = []
ecommon (x:xs) (y:ys)
  | x == y       = [x]
  | x < y        = ecommon xs ([y] ++ ys)
  | otherwise    = ecommon ([x] ++ xs) ys

graph1 = Gph [
               (1,  [2, 7])
             , (2,  [3])
             , (3,  [2, 8])
             , (4,  [5, 9])
             , (5,  [6])
             , (6,  [5, 11])
             , (7,  [1, 14])
             , (8,  [9, 16])
             , (9,  [10])
             , (10, [9, 11])
             , (11, [19, 10, 12, 6])
             , (12, [13, 20])
             , (13, [21])
             , (14, [7, 15])
             , (15, [16])
             , (16, [15, 17, 8])
             , (17, [18])
             , (18, [19])
             , (19, [22, 11])
             , (20, [23])
             , (21, [24, 13])
             , (22, [23])
             , (24, [23])
             ]


-- Check if a list of nodes is an independent subset of a graph
independent :: Graph -> [Node] -> Bool
independent g xs
  | (edistinct innodes gnodes) == [] &&
    (ecommon all gedges) == []          = True
  | otherwise                           = False
  where
    innodes = rdup (qs xs)
    all = (nedges innodes)
    gedges = rdup (qs (edges g))
    gnodes = rdup (qs (nodes g))

-- Distinct possible edges with all edges
edistinct :: Ord a => [a] -> [a] -> [a]
edistinct x [] = x
edistinct [] _ = []
edistinct (x:xs) (y:ys)
  | x == y     = edistinct xs ys
  | x < y      = [x] ++ edistinct xs ([y] ++ ys)
  | otherwise  = edistinct ([x] ++ xs) ys

-- Check if a list of nodes is a cover of a graph
cover :: Graph -> [Node] -> Bool
cover g xs
  | (edistinct innodes gnodes) == [] &&
    independent g comp                  = True
  | otherwise                           = False
  where
    innodes = rdup (qs xs)
    gnodes = rdup (qs (nodes g))
    comp = edistinct gnodes innodes
