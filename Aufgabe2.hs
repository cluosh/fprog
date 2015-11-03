-----------
-- PART 1
-----------
type Pence              = Int
type Shilling           = Int
type PoundSterling      = Int
type Amount             = (PoundSterling,Shilling,Pence)

-- Selectors
pence :: Amount -> Pence
pence (_,_,p)                   = p
shilling :: Amount -> Shilling
shilling (_,s,_)                = s
poundSterling :: Amount -> PoundSterling
poundSterling (ps,_,_)  	= ps

-- Check if amount of money is valid
isSound :: Amount -> Bool
isSound a
        | pence a >= 0 && 
		shilling a >= 0 && 
		poundSterling a >= 0	= True
        | otherwise			= False
        
-- Normalize the money
normalize :: Amount -> Amount
normalize a
        | isSound a	= (mod (pence a) 20, mod s 12, ps)
        | otherwise     = a
        where
                s = shilling a + (div (pence a) 20)
                ps = poundSterling a + (div s 12)

-- Add money
add :: Amount -> Amount -> Amount
add x y
        | isSound x && isSound y        = normalize (poundSterling x + 
                                          poundSterling y, shilling x + 
                                          shilling y, pence x + pence y)
        | otherwise                     = error "Falsche Eingabe"
        
-- Convert money to pences
toPence :: Amount -> Pence
toPence a
	| isSound a	= ((poundSterling a * 12) + shilling a) * 20 +
			  pence a
	| otherwise	= 0

-- Interest rate
type InterestRate	= Float
type Years		= Int
type Period		= Years
interest :: Amount -> InterestRate -> Period -> Amount
interest a p j
        | isSound a && p >= 0 && j >= 1 = normalize (0, 0, ceiling
					  (b * (1 + p/100)^j + 0.5))
        | otherwise			= (0,0,0)
        where
		b :: Float
		b = fromIntegral (toPence a)

-----------
-- PART 2
-----------
curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a,b,c) = f a b c

-- Test
addCurry :: (Integer, Integer, Integer) -> Integer
addCurry (a,b,c) = a + b + c

addCurryC :: Integer -> Integer -> Integer -> Integer
addCurryC a b c = (curry3 addCurry) a b c

-----------
-- PART 3
-----------
type DistanceAtDetection 	= Float -- Einheit: km
type Speed 			= Float -- km pro s
type TimeBeforeCollision 	= Integer -- s
type Distance 			= Integer -- km

dist :: DistanceAtDetection -> Speed -> Speed -> TimeBeforeCollision -> 
	Distance
dist d v1 v2 t
	| d >= 0 && v1 >= 0 && 
	  v2 >= 0 && t >= 0	= 0
	| otherwise		= -1

-----------
-- PART 4
-----------
type Wochentag 		= String
type Schaltjahr 	= Bool
type ErsterJaenner 	= Wochentag
type Tag 		= Int
type Monat 		= Int
type Datum 		= (Tag,Monat)

wochentag2 :: ErsterJaenner -> Schaltjahr -> Datum -> Wochentag
wochentag2 ej sj d = "None"
