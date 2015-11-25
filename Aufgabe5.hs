module Aufgabe5 where

-----------
-- PART 1
-----------

data Zeichen = A | B | C | D | E | F | G | H | I | J | K | L | M | N | 
               O | P | Q | R | S | T | U | V | W | X | Y | Z
               deriving (Eq,Ord,Show,Read,Enum,Bounded)
data Ziffer  = Null | Eins | Zwei | Drei | Vier | Fuenf | Sechs | 
               Sieben | Acht | Neun 
               deriving (Eq,Ord,Show,Read,Enum,Bounded)
newtype Nat  = Nat ((Zeichen,Zeichen,Zeichen),(Ziffer,Ziffer,Ziffer))

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
natFromInteger a = Nat ((b,c,d),(e,f,g))
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
  (+) a b
	| res < integerFromNat max             = fromInteger res
	| otherwise                            = max
	where
	  res = integerFromNat a + 
	        integerFromNat b
	  max = Nat ((Z,Z,Z),(Neun,Neun,Neun))
  (*) a b
    | res < integerFromNat max             = fromInteger res
	| otherwise                            = max
	where
	  res = integerFromNat a *
	        integerFromNat b
	  max = Nat ((Z,Z,Z),(Neun,Neun,Neun))
  (-) a b
    | res > 0                              = fromInteger res
    | otherwise                            = 0
    where
      res = integerFromNat a -
            integerFromNat b
  signum a
	| a > zero  = 1
	| otherwise = 0
	where
	  zero      = fromInteger 0
  abs a         = a
  negate a      = fromInteger 0
  fromInteger a = natFromInteger a

-- Implementing minimum enum instance for Nat
instance Enum Nat where
  toEnum a      = fromInteger (toInteger a)
  fromEnum a    = fromIntegral (toInteger a)
  
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
  toInteger a   = integerFromNat a

-----------
-- PART 2
-----------
type PosRat = (Nat, Nat)

-- Define ggt on Nat
ggT :: Int -> Int -> Int
ggT a b
  | b > 0  = ggT b (rem a b)
  | b <= 0 = a

-- Check if a rational number is canonical
isCanPR :: PosRat -> Bool
isCanPR (a,b)
  | b > 0 && g == 1 = True
  | otherwise       = False
  where
    g = ggT (fromEnum a) (fromEnum b)

-- Make a rational number canoncial
mkCanPR :: PosRat -> PosRat
mkCanPR (a,b)
  | b > 0     = (div a g, div b g)
  | otherwise = (fromInteger 0, fromInteger 0)
  where
    g = toEnum (ggT (fromEnum a) (fromEnum b))

-- Addition of rational numbers
plusPR :: PosRat -> PosRat -> PosRat
plusPR (a,b) (x,y)
  | b > 0 && y > 0 = mkCanPR (a1 * y1 + x1 * b1, b1 * y1)
  | otherwise      = (0,0)
  where
    (a1,b1) = mkCanPR (a,b)
    (x1,y1) = mkCanPR (x,y)
  
-- Subtraction of rational numbers
minusPR :: PosRat -> PosRat -> PosRat
minusPR (a,b) (x,y)
  | b > 0 && y > 0 = mkCanPR (a * a1 - x * x1, toEnum kgv)
  | otherwise      = (0,0)
  where
    be = toInteger (fromEnum b)
    ye = toInteger (fromEnum y)
    g = ggT be ye
    kgv = div (be * ye) g
    a1 = div kgv be
    x1 = div kgv ye
  
-- Multiplication of rational numbers
timesPR :: PosRat -> PosRat -> PosRat
timesPR (a,b) (x,y)
  | b > 0 && y > 0 = mkCanPR (a1 * x1,b1 * y1)
  | otherwise      = (0,0)
  where
    (a1,b1) = mkCanPR (a,b)
    (x1,y1) = mkCanPR (x,y)
  
-- Division of rational numbers
divPR :: PosRat -> PosRat -> PosRat
divPR (a,b) (x,y)
  | b > 0 && y > 0 = timesPR (a,b) (y,x)
  | otherwise      = (0,0)

-- Check if two PosRats are equal
eqPR :: PosRat -> PosRat -> Bool
eqPR (a,b) (x,y)
  | b > 0 && y > 0 && ac == xc && bc == yc = True
  | otherwise                              = False
  where
    (ac,bc) = mkCanPR (a,b)
    (xc,yc) = mkCanPR (x,y)

-- Check if two PosRats are not equal
neqPR :: PosRat -> PosRat -> Bool
neqPR (a,b) (x,y)
  | b > 0 && y > 0 = not (eqPR (a,b) (x,y))
  | otherwise      = False

-- Check if a is bigger than b
grPR :: PosRat -> PosRat -> Bool
grPR (a,b) (x,y)
  | b > 0 && y > 0 = ac1 > bc1
  | otherwise      = False
  where
    ac  = mkCanPR (a,b)
    bc  = mkCanPR (x,y)
    ac1 = (toInteger (fst ac)) * (toInteger (snd bc))
    bc1 = (toInteger (fst bc)) * (toInteger (snd ac))

-- Check if a is less than b
lePR :: PosRat -> PosRat -> Bool
lePR (a,b) (x,y)
  | b > 0 && y > 0 = not (eqPR (a,b) (x,y)) && not (grPR (a,b) (x,y))
  | otherwise      = False

-- Check if a is bigger or equahl than b
grEqPR :: PosRat -> PosRat -> Bool
grEqPR (a,b) (x,y)
  | b > 0 && y > 0 = not (lePR (a,b) (x,y))
  | otherwise      = False

-- Check if a is less or equal than b
leEqPR :: PosRat -> PosRat -> Bool
leEqPR (a,b) (x,y)
  | b > 0 && y > 0 = not (grPR (a,b) (x,y))
  | otherwise      = False
