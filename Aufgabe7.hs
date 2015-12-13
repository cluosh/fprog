module Aufgabe7 where

-----------
-- PART 1
-----------
type Node = Integer
type Edge = (Node,Node)
type Source = Node
type Sink = Node
newtype Graph = Gph [(Source,[Sink])] deriving (Eq,Show)
data Zeichen = A | B | C | D | E | F | G | H | I | J | K | L | M | N | 
               O | P | Q | R | S | T | U | V | W | X | Y | Z
               deriving (Eq,Ord,Show,Read,Enum,Bounded)
data Ziffer = Null | Eins | Zwei | Drei | Vier | Fuenf | Sechs | 
              Sieben | Acht | Neun 
              deriving (Eq,Ord,Show,Read,Enum,Bounded)
newtype Nat = Nat ((Zeichen,Zeichen,Zeichen),(Ziffer,Ziffer,Ziffer))
              deriving (Eq,Ord,Show)

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

-- Subsequences of size n
zapWith f    xs     []  = xs
zapWith f (x:xs) (y:ys) = f x y : zapWith f xs ys
filterCombs :: ([a] -> Bool) -> Int -> [a] -> [[a]]
filterCombs p n xs | n > length xs = [] 
filterCombs p n xs = go xs id !! n where
    go    []  ds = [[[]]]
    go (x:xs) ds
        | p (ds' []) = zapWith (++) ([] : map (map (x:)) with) without
        | otherwise  = without
        where
            ds'     = ds . (x:)
            with    = go xs ds'
            without = go xs ds
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

-- Get all nodes of a graph, including duplicates
nodes :: Graph -> [Node]
nodes (Gph [])     = []
nodes (Gph (x:xs)) = (nodes (Gph xs)) ++ (snd x) ++ [fst x]

-- Get all edges of a graph, including duplicates
edges :: Graph -> [Edge]
edges (Gph [])     = []
edges (Gph (x:xs)) = (edges (Gph xs)) ++ 
                     [(min y z, max y z) | y <- [fst x], z <- (snd x)]

-- Create all edges from nodes (for cover)
nedges :: [Node] -> [Edge]
nedges []     = []
nedges (x:xs) = [(x,y) | y <- ([x] ++ xs)] ++ nedges xs

-- Create all edges from nodes (for independent)
nedges2 :: [Node] -> [Edge]
nedges2 []     = []
nedges2 (x:xs) = [(x,y) | y <- xs] ++ nedges2 xs

-- Common possible edges with all edges
ecommon :: [Edge] -> [Edge] -> [Edge]
ecommon _ []     = []
ecommon [] _     = []
ecommon (x:xs) (y:ys)
  | x == y       = [x]
  | x < y        = ecommon xs ([y] ++ ys)
  | otherwise    = ecommon ([x] ++ xs) ys

-- Check if a list of nodes is an independent subset of a graph
independent :: Graph -> [Node] -> Bool
independent _ []                        = True
independent g xs
  | (edistinct innodes gnodes) == [] &&
    (ecommon all gedges) == []          = True
  | otherwise                           = False
  where
    innodes = rdup (qs xs)
    all = (nedges2 innodes)
    gedges = rdup (qs (edges g))
    gnodes = rdup (qs (nodes g))

-- Check if a list of nodes is an independent subset of a graph
independent2 :: [Node] -> [Edge] -> [Node] -> Bool
independent2 _ _ []           = True
independent2 n e xs
  | (edistinct xs n) == [] &&
    (ecommon all e) == []     = True
  | otherwise                 = False
  where
    all = (nedges2 xs)

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
    (ecommon all gedges) == []          = True
  | otherwise                           = False
  where
    innodes = rdup (qs xs)
    gnodes = rdup (qs (nodes g))
    gedges = rdup (qs (edges g))
    comp = edistinct gnodes innodes
    all = nedges (rdup (qs comp))
    
-- Check all subsets with size k for independency
sos :: Int -> ([Node] -> Bool) -> [Node] -> [[Node]]
sos n p xs = let l = length xs
             in if n > l then [] else sbs p xs !! (l-n)
 where
   sbs _ []     = [[[]]]
   sbs p (x:xs) = let next = sbs p xs
                  in zipWith (++) ([]:next) 
                     (map (filter p) (map (map (x:)) next ++ [[]]))

-- Wrapper for k independency
kIndependent :: Graph -> Nat -> [[Node]]
kIndependent g k = let n = rdup (qs (nodes g))
                       e = rdup (qs (edges g))
                       ki = fromIntegral (integerFromNat k)
                   in qs (sos ki (independent2 n e) n)

-- Use depth first search to check for connectedness
dfs :: Graph -> [Edge] -> [Node]
dfs (Gph []) []          = []
dfs (Gph (x:xs)) []      = []
dfs (Gph []) (top:stack) = [snd top]
dfs (Gph xs) (top:stack)
  | filterResult /= []   = [fst x] ++ (dfs (Gph negFilter) 
                                           (nedges ++ stack))
  | otherwise            = [snd top] ++ (dfs (Gph xs) stack)
  where
    filterResult = filter ((==(snd top)).fst) xs
    negFilter    = filter ((/=(snd top)).fst) xs
    x            = head filterResult
    nedges       = [(y, z) | y <- [fst x], z <- (snd x)]

-- Check for unconnected points
unconnected :: Graph -> [Node]
unconnected (Gph [])   = []
unconnected (Gph (x:xs))
  | (snd x) == []      = [fst x] ++ unconnected (Gph xs)
  | otherwise          = unconnected (Gph xs)

-- Check if a graph is connected
isConnected :: Graph -> Bool
isConnected (Gph []) = True
isConnected (Gph xs)
  | unconnected (Gph xs) /= [] = False
  | otherwise                  = cover (Gph xs) dnodes
  where
    dnodes = rdup (qs (dfs (Gph xs) fedges))
    x      = head xs
    fedges = [(y, z) | y <- [fst x], z <- (snd x)]

-- Check if a graph is a tree
isTree :: Graph -> Bool
isTree (Gph []) = True
isTree (Gph xs)
  | isConnected (Gph xs) = True
  | otherwise            = False

-----------
-- PART 2
-----------

data Airports    = AMS | AUC | DUS | FRA | HAJ | HAM | JFK | LAX | MUC | 
                   SFO | TXL | VIE 
                   deriving (Eq,Ord,Bounded,Enum,Show)
data Airlines    = Aeroflot | AirBerlin | AirFrance | AUA | 
                   BritishAirways | Delta | GermanWings | KLM | 
                   Lufthansa | Swiss
                   deriving (Eq,Ord,Bounded,Enum,Show)
type Fare        = Nat
type TotalFare   = Fare
type Networks    = Airlines -> Airports -> [(Airports,Fare)]
type Alliances   = Airlines -> [Airlines]
type Origin      = Airports
type Destination = Airports
type Relation    = (Origin,Airlines,Destination)
type Connection  = [Relation]

nat0   = Nat ((A, A, A), (Null, Null, Null))
nat1   = Nat ((A, A, A), (Null, Null, Eins))
nat2   = Nat ((A, A, A), (Null, Null, Zwei))
nat3   = Nat ((A, A, A), (Null, Null, Drei))
nat4   = Nat ((A, A, A), (Null, Null, Vier))
nat5   = Nat ((A, A, A), (Null, Null, Fuenf))
nat8   = Nat ((A, A, A), (Null, Null, Acht))
nat12  = Nat ((A, A, A), (Null, Eins, Zwei))
nat13  = Nat ((A, A, A), (Null, Eins, Drei))
nat15  = Nat ((A, A, A), (Null, Eins, Fuenf))
nat20  = Nat ((A, A, A), (Null, Zwei, Null))
nat23  = Nat ((A, A, A), (Null, Zwei, Drei))
nat50  = Nat ((A, A, A), (Null, Fuenf, Null))
nat100 = Nat ((A, A, A), (Eins, Null, Null))

noFlyZone :: Networks
noFlyZone _ _ = []

network :: Networks
network Aeroflot = aeroflot
network AUA = aua
network _ = (\_ -> [])

extendedNetwork :: Networks
extendedNetwork KLM = klm
extendedNetwork airline = network airline

aeroflot :: Airports -> [(Airports, Fare)]
aeroflot AMS = [(HAM, nat2), (TXL, nat3)]
aeroflot AUC = [(LAX, nat4), (SFO, nat15)]
aeroflot DUS = [(FRA, nat1), (HAM, nat2)]
aeroflot FRA = [(DUS, nat1), (JFK, nat2), (HAJ, nat1), (MUC, nat2), (VIE, nat1)]
aeroflot HAJ = [(HAM, nat1), (FRA, nat1), (VIE, nat3)]
aeroflot HAM = [(AMS, nat1), (DUS, nat2), (HAJ, nat1), (TXL, nat1)]
aeroflot JFK = [(HAM, nat5), (LAX, nat20), (FRA, nat2), (SFO, nat2)]
aeroflot LAX = [(AUC, nat4), (JFK, nat20), (LAX, nat50) ,(SFO, nat23),
                (TXL, nat4)]
aeroflot MUC = [(FRA, nat2), (SFO, nat3)]
aeroflot SFO = [(AUC, nat15), (JFK, nat2), (LAX, nat23), (MUC, nat3),
                (VIE, nat1)]
aeroflot TXL = [(AMS, nat3), (HAM, nat1), (LAX, nat4), (VIE, nat4)]
aeroflot VIE = [(FRA, nat1), (HAJ, nat3), (SFO, nat1), (TXL, nat4)]

aua :: Airports -> [(Airports, Fare)]
aua LAX = [(VIE, nat100)]
aua _   = []

klm :: Airports -> [(Airports, Fare)]
klm AUC = [(LAX, nat3)]
klm DUS = [(JFK, nat3), (VIE, nat3)]
klm HAM = [(DUS, nat2)]
klm JFK = [(AUC, nat3), (VIE, nat1)]
klm LAX = [(HAM, nat1)]
klm _   = []

allianceAeroflotAUA :: Airlines -> [Airlines]
allianceAeroflotAUA Aeroflot = [AUA]
allianceAeroflotAUA AUA      = [Aeroflot]
allianceAeroflotAUA _        = []

allianceAll :: Airlines -> [Airlines]
allianceAll _ = [minBound :: Airlines .. maxBound :: Airlines]

-- Get all intermediate connections from a single airport/airline
relOrigin :: Origin -> Networks -> Airlines -> [Relation]
relOrigin o n al = map (\(x,y) -> (o,al,x)) $ n al o

-- Get all connections from one airport to a destination with 
-- a single airline
allToDest :: Origin -> Destination -> Networks -> 
             Airlines -> [Airports] -> [Relation]
allToDest o d n al visited
  | dest == [] = next rel ++ rel
  | otherwise  = dest
  where
	rel    = relOrigin o n al
	nvisit = visited ++ map (\(x,y,z) -> z) rel
	dest   = filter (\(x,y,z) -> z == d) rel
	ochk r = filter (\(x,y,z) -> not $ elem z visited) r
	next r = foldl (++) [] $ 
	         [allToDest z d n al nvisit | (x,y,z) <- ochk r]
	
-- Minimum layover
airlineConnections :: Origin -> Destination -> Networks -> 
                      Airlines -> [Connection]
airlineConnections _ _ _ _ = []

-- Cheapest flight within alliance
allianceConnections :: Origin -> Destination -> Networks -> 
                       Alliances -> Airlines -> [Connection]
allianceConnections _ _ _ _ _ = []

-- Cheapest flight overall
cheapestConnections :: Origin -> Destination -> Networks -> 
                       [(Connection,TotalFare)]
cheapestConnections _ _ _ = []
