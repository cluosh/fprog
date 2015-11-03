-----------
-- PART 1
-----------
type Pence         = Int
type Shilling      = Int
type PoundSterling = Int
type Amount        = (PoundSterling,Shilling,Pence)

-- Selectors
pence         :: Amount -> Pence
shilling      :: Amount -> Shilling
poundSterling :: Amount -> PoundSterling
pence (_,_,p)          = p
shilling (_,s,_)       = s
poundSterling (ps,_,_) = ps

-- Check if amount of money is valid
isSound :: Amount -> Bool
isSound (a,b,c)
  | a >= 0 && b >= 0 && c >= 0 = True
  | otherwise                  = False
        
-- Normalize the money
normalize :: Amount -> Amount
normalize a
  | isSound a = (ps, mod s 12, mod (pence a) 20)
  | otherwise = a
  where
    s = shilling a + (div (pence a) 20)
    ps = poundSterling a + (div s 12)

-- Add money
add :: Amount -> Amount -> Amount
add (a,b,c) (x,y,z)
  | isSound (a,b,c) && isSound (x,y,z) = normalize (a+x, b+y, c+z)
  | otherwise                          = error "Falsche Eingabe"
        
-- Convert money to pences
toPence :: Amount -> Pence
toPence (a,b,c)
  | isSound (a,b,c) = ((a * 12) + b) * 20 + c
  | otherwise       = 0

-- Interest rate
type InterestRate = Float
type Years        = Int
type Period       = Years
interest :: Amount -> InterestRate -> Period -> Amount
interest a p j
  | isSound a && 
    p >= 0 && 
    j >= 1       = normalize (0, 0, floor (b * (1 + p)^j + 0.5))
  | otherwise    = (0,0,0)
  where
    b :: Float
    b = fromIntegral (toPence a)

-----------
-- PART 2
-----------

-- Curry function in the style of curry for 3 arguments
curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

-- Uncurry function in the style of uncurry for 3 arguments
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a,b,c) = f a b c

-- Testing unfctions
addCurry :: (Integer, Integer, Integer) -> Integer
addCurry (a,b,c) = a + b + c
addCurryC :: Integer -> Integer -> Integer -> Integer
addCurryC a b c = (curry3 addCurry) a b c

-----------
-- PART 3
-----------
type DistanceAtDetection = Float
type Speed               = Float
type TimeBeforeCollision = Integer
type Distance            = Integer

-- Calculate distance at time away from collision
dist :: DistanceAtDetection -> Speed -> Speed -> TimeBeforeCollision -> 
        Distance
dist d v1 v2 t
  | d >= 0 && v1 >= 0 && 
    v2 >= 0 && t >= 0 && 
    tcheck >= fromIntegral t = floor(sol + 0.5)
  | otherwise                = -1
  where
    tcheck = d / (v1 + v2)
    sol = (v1 * fromIntegral t) + (v2 * fromIntegral t)

-----------
-- PART 4
-----------
type Wochentag          = String
type Schaltjahr         = Bool
type ErsterJaenner      = Wochentag
type Tag                = Int
type Monat              = Int
type Datum              = (Tag,Monat)

-- Extensive use of pattern matching for checking if date is valid
-- Probably not a good idea
checkDate :: Datum -> Schaltjahr -> Bool
checkDate (d,4) _
  | d >= 1 && d <= 30 = True
  | otherwise         = False
checkDate (d,6) _  = checkDate(d,4) False
checkDate (d,9) _  = checkDate(d,4) False
checkDate (d,11) _ = checkDate(d,4) False
checkDate (d,2) sj
  | sj && d >= 1 && d <= 29 = True
  | d > 0 && d <= 28        = True
  | otherwise               = False
checkDate (d,m) _
  | m > 0 && d > 0 && d <= 31  = True
  | otherwise                  = False

-- Sum up days till given date
sumDate :: Datum -> Schaltjahr -> Int
sumDate (d,m) sj
  | not sj && m > 2 = sum (take (m-1) days) + d - 1
  | otherwise    = sum (take (m-1) days) + d
  where
    days = [31,29,31,30,31,30,31,31,30,31,30,31]

-- Days of the week lookup function
days :: String -> Int
days "Sonntag"    = 0
days "Montag"     = 1
days "Dienstag"   = 2
days "Mittwoch"   = 3
days "Donnerstag" = 4
days "Freitag"    = 5
days "Samstag"    = 6
days "Sonnabend"  = 6
days _            = -1

-- Index to days of the week
dind :: Int -> String
dind 0 = "Sonntag"
dind 1 = "Montag"
dind 2 = "Dienstag"
dind 3 = "Mittwoch"
dind 4 = "Donnerstag"
dind 5 = "Freitag"
dind 6 = "Samstag"

-- Get day of the week by given date
wochentag2 :: ErsterJaenner -> Schaltjahr -> Datum -> Wochentag
wochentag2 ej sj d
  | checkDate d sj && e >= 0 = dind (mod (e - 1 + sumDate d sj) 7)
  | otherwise                = "Falsche Argumente"
  where
    e = days ej
