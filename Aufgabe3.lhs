-----------
  PART 1
-----------

> type Wochentag          = String
> type Schaltjahr         = Bool
> type ErsterJaenner      = Wochentag
> type Tag                = Int
> type Monat              = Int
> type Datum              = (Tag,Monat)

Extensive use of pattern matching for checking if date
is valid, probably not a good idea

> checkDate :: Datum -> Schaltjahr -> Bool
> checkDate (d,4) _
>  | d >= 1 && d <= 30 = True
>  | otherwise         = False
> checkDate (d,6) _  = checkDate(d,4) False
> checkDate (d,9) _  = checkDate(d,4) False
> checkDate (d,11) _ = checkDate(d,4) False
> checkDate (d,2) sj
>  | sj && d >= 1 && d <= 29 = True
>  | d > 0 && d <= 28        = True
>  | otherwise               = False
> checkDate (d,m) _
>  | m > 0 && m <= 12 && 
>    d > 0 && d <= 31        = True
>  | otherwise               = False

Sum up days till given date

> sumDate :: Datum -> Schaltjahr -> Int
> sumDate (d,m) sj
>  | not sj && m > 2 = sum (take (m-1) days) + d - 1
>  | otherwise    = sum (take (m-1) days) + d
>  where
>    days = [31,29,31,30,31,30,31,31,30,31,30,31]

Days of the week lookup function

> days :: String -> Int
> days "Sonntag"          = 0
> days "Montag"           = 1
> days "Dienstag"         = 2
> days "Mittwoch"         = 3
> days "Donnerstag"       = 4
> days "Freitag"          = 5
> days "Samstag"          = 6
> days "Sonnabend"        = 6
> days _                  = -1

Index to days of the week

> dind :: Int -> String
> dind 0 = "Sonntag"
> dind 1 = "Montag"
> dind 2 = "Dienstag"
> dind 3 = "Mittwoch"
> dind 4 = "Donnerstag"
> dind 5 = "Freitag"
> dind 6 = "Samstag"

Find the day of the week of a given date by another given
date and the day of the week for the other given date

> wochentag3 :: Datum -> Wochentag -> Schaltjahr -> Datum ->
>               Wochentag
> wochentag3 d1 wt sj d2
>   | checkDate d1 sj &&
>     checkDate d2 sj &&
>     d >= 0             = dind (mod (d + sumDate d2 sj -
>                            sumDate d1 sj) 7)
>   | otherwise          = "Falsche Argumente"
>   where
>     d = days wt

-----------
  PART 2
-----------

Count the number of lower vocal letters in a string

> lower :: String -> Int
> lower xs = length (filter (\x -> x == 'a' || x == 'i' ||
>                      x == 'u' || x == 'o' || x == 'e') xs)

Simple quicksort

qs :: [String] -> [String]
qs []     = []
qs (x:xs) = qs (filter (< x) xs) ++ [x] ++
            qs (filter (>= x) xs)

> qs :: Ord a => [a] -> [a]
> qs []     = []
> qs (x:xs) = qs (filter (< x) xs) ++ [x] ++
>             qs (filter (>= x) xs)
        
Get element frequencies from list

> efreq :: Integer -> [String] -> [(Integer,String)]
> efreq _ []        = []
> efreq n (x:[])    = [(n,x)]
> efreq n (x:xs)
>   | x /= head xs = [(n,x)] ++ efreq 1 xs
>   | otherwise    = efreq (n+1) xs

Count number of distinct elements with more or equal
than three letters

> pwv :: [String] -> Int
> pwv xs = length (efreq 1 (qs (filter
>           (\x -> lower x >= 3) xs)))

-----------
  PART 3
-----------

Selector functions for triple

> fs :: (a,b,c) -> a
> fs (a,_,_) = a
> sn :: (a,b,c) -> b
> sn (_,b,_) = b
> tr :: (a,b,c) -> c
> tr (_,_,c) = c

Count frequency of strings in a list, with order

> freq :: [(String,Integer)] -> (Integer, String, [Integer]) -> 
>         [(Integer, String, [Integer])]
> freq [] _             = []
> freq (x:[]) (a,b,c)   = [(a + 1,b,c ++ [snd x])]
> freq (x:xs) (a,b,c)
>   | fst x /= fst (head xs) = [(a + 1,b, c ++ [snd x])] ++
>                              freq xs (0,fst (head xs),[])
>   | otherwise              = freq xs (a + 1,b,c ++ [snd x])

Expand the previously created frequency table again

> expand :: [(Integer, String, [Integer])] -> Integer -> 
>           [(Integer, String)]
> expand [] _     = []
> expand (x:xs) i
>   | fs x == i   = [(y,z) | z <- [sn x], y <- tr x] ++ expand xs i
>   | otherwise   = expand xs i

Implement the streamline function using several quicksorts and
a frequency table with the saved position of the characters

> streamline :: [String] -> Int -> [String]
> streamline [] _ = []
> streamline xs n
>   | n > 0       = [snd x | x <- exp]
>   | otherwise   = []
>   where
>     ni = toInteger n
>     sorted = qs (zip xs [0..])
>     freql = freq sorted (0,fst (head sorted), [])
>     exp = qs (expand freql ni)

-----------
  PART 4
-----------

> type Kennzeichen = ((Char,Char,Char),(Int,Int,Int))

Add n to a triplet of chars

> addChar :: Int -> (Char,Char,Char) -> (Char,Char,Char)
> addChar n (x,y,z) = (toEnum (mod a 26 + 65) :: Char,
>                     toEnum (mod b 26 + 65) :: Char,
>                     toEnum (mod c 26 + 65) :: Char)
>   where
>     c = fromEnum z - 65 + n
>     b = fromEnum y - 65 + div c 26
>     a = fromEnum x - 65 + div b 26

Add n to a triplet of integers and return a "carry flag" as
first element of a tuple

> addInt :: Int -> (Int,Int,Int) -> (Int, (Int,Int,Int))
> addInt n (x,y,z) = (div a 10, (mod a 10, mod b 10, mod c 10))
>   where
>     c = z + n
>     b = y + div c 10
>     a = x + div b 10

Check if an entered license plate is valid

> valid :: Kennzeichen -> Bool
> valid ((a,b,c),(x,y,z))
>   | x >= 0 && y >= 0 && z >= 0 &&
>     x <= 9 && y <= 9 && z <= 9 &&
>     ae >= 65 && be >= 65 && ce >= 65 &&
>     ae <= 90 && be <= 90 && ce <= 90    = True
>   | otherwise                           = False
>   where
>     ae = fromEnum a
>     be = fromEnum b
>     ce = fromEnum c

Return the next license plate

> nf :: Kennzeichen -> Kennzeichen
> nf ((a,b,c),(x,y,z))
>   | valid ((a,b,c),(x,y,z)) = (charRes, snd intRes)
>   | otherwise               = ((a,b,c),(x,y,z))
>   where
>     intRes = addInt 1 (x,y,z)
>     charRes = addChar (fst intRes) (a,b,c)

Return the previous license plate

> vg :: Kennzeichen -> Kennzeichen
> vg ((a,b,c),(x,y,z))
>   | valid ((a,b,c),(x,y,z)) = (charRes, snd intRes)
>   | otherwise               = ((a,b,c),(x,y,z))
>   where
>     intRes = addInt (-1) (x,y,z)
>     charRes = addChar (fst intRes) (a,b,c)
