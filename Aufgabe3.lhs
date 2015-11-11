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
>  | m > 0 && d > 0 && d <= 31  = True
>  | otherwise                  = False

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

Count frequency of strings in a list, but don't alter the
list (TODO)

> freq :: Integer -> [(String, Integer)] ->
>         [(Integer,Integer,String)]
> freq _ []         = []
> freq n (x:[])     = [(snd x, n, fst x)]
> freq n (x:xs)
>   | fst x /= fst (head xs)  = [(snd x, n, fst x)] ++ freq 1 xs
>   | otherwise               = freq (n+1) xs

Selector functions for triple

> fs :: (a,b,c) -> a
> fs (a,_,_) = a
> sn :: (a,b,c) -> b
> sn (_,b,_) = b
> tr :: (a,b,c) -> c
> tr (_,_,c) = c

Create a frequency table (including number of original position)
and insert 

> streamline :: [String] -> Int -> [String]
> streamline xs n
>  | n > 0     = [tr x  | x <- (filter ((==ni).sn) (qs (freq 1
>                (qs (zip xs [0..])))))]
>  | otherwise = []
>  where
>    ni = toInteger n

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
>     c = fromEnum x - 65 + n
>     b = fromEnum y - 65 + div c 26
>     a = fromEnum z - 65 + div b 26

Add n to a triplet of integers and return a "carry flag" as
first element of a tuple

> addInt :: Int -> (Int,Int,Int) -> (Int, (Int,Int,Int))
> addInt n (x,y,z) = (div a 10, (mod a 10, mod b 10, mod c 10))
>   where
>     c = x + n
>     b = y + div c 10
>     a = z + div b 10

Return the next license plate

> nf :: Kennzeichen -> Kennzeichen
> nf ((a,b,c),(x,y,z)) = (charRes, snd intRes)
>   where
>     intRes = addInt 1 (x,y,z)
>     charRes = addChar (fst intRes) (a,b,c)

Return the previous license plate

> vg :: Kennzeichen -> Kennzeichen
> vg ((a,b,c),(x,y,z)) = (charRes, snd intRes)
>   where
>     intRes = addInt (-1) (x,y,z)
>     charRes = addChar (fst intRes) (a,b,c)
