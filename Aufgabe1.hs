module Aufgabe1 where

-----------
-- PART 1
-----------
type Wochentag     = String
type Schaltjahr    = Bool
type ErsterJaenner = Wochentag
type NterTagImJahr = Int

-- Choose the day of the week for a given 1st January and number of
-- days passed since then
wochentag :: ErsterJaenner -> Schaltjahr -> NterTagImJahr -> Wochentag
wochentag ej sj ntij
        | e >= 0 && ntij > 0 && ntij <= u = dind (mod (e + ntij - 1) 7)
        | otherwise                       = "Falsche Argumente"
        where
                e = days ej
                u = if sj then 366 else 365

-- Days of the week lookup function
days :: String -> Int
days "Sonntag"          = 0
days "Montag"           = 1
days "Dienstag"         = 2
days "Mittwoch"         = 3
days "Donnerstag"       = 4
days "Freitag"          = 5
days "Samstag"          = 6
days "Sonnabend"        = 6
days _                  = -1

-- Index to days of the week
dind :: Int -> String
dind 0 = "Sonntag"
dind 1 = "Montag"
dind 2 = "Dienstag"
dind 3 = "Mittwoch"
dind 4 = "Donnerstag"
dind 5 = "Freitag"
dind 6 = "Samstag"


-----------
-- PART 2
-----------

-- Calculate variation, curried, use foldl for faculty
-- Calculation by using simpler form of n!/(n-r)!
vc :: Integer -> Integer -> Integer
vc n r
        | n >= r && r >= 0      = foldl (*) 1 [n-r+1..n]
        | otherwise                     = -1

-- Calculate variation, uncurried, by using uncurry function
vuc :: (Integer, Integer) -> Integer
vuc = uncurry vc


-----------
-- PART 3
-----------

-- Use a list with letter frequencies to build a new string containing
-- the letters sorted by their frequency
frequencySort :: String -> String
frequencySort s = ts (freq 1 (qs s))

-- Get the correct frequency table from a previously created
-- (Char,Integer) list. The previously created list should be sorted
-- by the characters and numbered
freq :: Integer -> String -> [(Char,Integer)]
freq _ []                       = []
freq n (x:[])           = [(x,n)]
freq n (x:xs)
        | x /= head xs  = [(x,n)] ++ freq 1 xs
        | otherwise             = freq (n+1) xs

-- A simple quicksort for sorting letters
qs :: String -> String
qs []                   = []
qs (x:xs)               = qs (filter (< x) xs) ++ [x] ++ 
                          qs (filter (>= x) xs)

-- Simple tuple quicksort and replication               
ts :: [(Char,Integer)] -> String
ts []                   = []
ts ((x,y):xs)   = ts (filter ((< y).snd) xs) ++ repI x y ++
                                        ts (filter ((>= y).snd) xs)
                                        
-- Replicate for Integer
repI :: Char -> Integer -> String
repI _ 0 = []
repI c n = [c] ++ repI c (n-1)

-----------
-- PART 4
-----------

-- Convert an integer to an octal string
octal :: Integer -> String
octal 0 = []
octal n = octal (div n 8) ++ show (mod n 8)

-- Check if the octal representation of an integer is a palindrome
pcheck :: Integer -> Bool
pcheck n = reverse s == s where s = octal (abs n)
