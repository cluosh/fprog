> module Aufgabe4 where

-----------
-- PART 1
-----------

Definition of the data types

> data Zeichen = A | B | C | D | E | F | G | H | I | J | K | L | M | N | 
>                O | P | Q | R | S | T | U | V | W | X | Y | Z 
>                deriving (Eq,Ord,Show,Enum)
> data Ziffer  = Null | Eins | Zwei | Drei | Vier | Fuenf | Sechs |
>                Sieben | Acht | Neun 
>                deriving (Eq,Ord,Show,Enum)
> newtype Nat =  Nat ((Zeichen,Zeichen,Zeichen),
>                     (Ziffer, Ziffer, Ziffer))
>                deriving (Eq,Show)

Convert a letter to an Integer

> ztoi :: Zeichen -> Integer
> ztoi a = toInteger (fromEnum a)

Convert a digit to an Integer

> dtoi :: Ziffer -> Integer
> dtoi a = toInteger (fromEnum a)

Convert Integer to a letter

> itoz :: Integer -> Zeichen
> itoz a = toEnum (fromInteger a)

Convert an Integer to a digit

> itod :: Integer -> Ziffer
> itod a = toEnum (fromInteger a)

Convert a Nat to an Integer

> ntoi :: Nat -> Integer
> ntoi (Nat ((a,b,c),(d,e,f))) = x + y + z + u + v + w
>   where
>     x = dtoi f
>     y = 10 * (dtoi e)
>     z = 100 * (dtoi d)
>     u = 1000 * (ztoi c)
>     v = 26000 * (ztoi b)
>     w = 676000 * (ztoi a)

Convert Integer to a Nat

> iton :: Integer -> Nat
> iton a = Nat ((itoz w, itoz v, itoz u),(itod z, itod y, itod x))
>   where
>     x = mod a 10
>     y = div (mod a 100 - x) 10
>     z = div (mod a 1000 - y - x) 100
>     u = div (mod a 26000 - z - y - x) 1000
>     v = div (mod a 676000 - u - z - y - x) 26000
>     w = div (a - v - u - z - y - x) 676000

Add two Nat numbers

> plusN :: Nat -> Nat -> Nat
> plusN a b
>   | res < ntoi max = iton res
>   | otherwise      = max
>   where
>     res = ntoi a + ntoi b
>     max = (Nat ((Z,Z,Z),(Neun,Neun,Neun)))

Subtract two Nat numbers

> minusN :: Nat -> Nat -> Nat
> minusN a b
>   | res >  0  = iton res
>   | otherwise = (Nat ((A,A,A),(Null,Null,Null)))
>   where
>     res = (ntoi a) - (ntoi b)

Multiply two Nat number

> timesN :: Nat -> Nat -> Nat
> timesN a b
>   | res < ntoi max = iton res
>   | otherwise      = max
>   where
>     res = (ntoi a) * (ntoi b)
>     max = (Nat ((Z,Z,Z),(Neun,Neun,Neun)))

Divide two Nat number

> divN :: Nat -> Nat -> Nat
> divN a b
>   | ib > 0    = iton res
>   | otherwise = error "Division durch 0 nicht moeglich"
>   where
>     ib = ntoi b
>     res = div (ntoi a) ib

Nat number modulo

> modN :: Nat -> Nat -> Nat
> modN a b
>   | ib > 0    = iton res
>   | otherwise = error "Division durch 0 nicht moeglich"
>   where
>     ib = ntoi b
>     res = mod (ntoi a) ib

Calculate power of Nat

> powerN :: Nat -> Nat -> Nat
> powerN _ (Nat ((A,A,A),(Null,Null,Null))) = 
>          (Nat ((A,A,A),(Null,Null,Eins)))
> powerN (Nat ((A,A,A),(Null,Null,Null))) _ = 
>          (Nat ((A,A,A),(Null,Null,Null)))
> powerN (Nat ((A,A,A),(Null,Null,Eins))) _ =
>          (Nat ((A,A,A),(Null,Null,Eins)))
> powerN a b
>   | bi <= lcheck && res < ntoi max = iton res
>   | otherwise                      = max
>   where
>     bi = ntoi b
>     ai = ntoi a
>     lcheck = ceiling (log (fromInteger (ntoi max)) / 
>                       log (fromInteger ai))
>     res = ai ^ bi
>     max = (Nat ((Z,Z,Z),(Neun,Neun,Neun)))

Check Nat numbers for equality

> eqN :: Nat -> Nat -> Bool
> eqN a b = a == b

Check Nat numbers for inequality

> neqN :: Nat -> Nat -> Bool
> neqN a b = not (eqN a b)

Check if Nat number is bigger than another Nat number

> grN :: Nat -> Nat -> Bool
> grN a b
>   | ntoi (minusN a b) > 0 = True
>   | otherwise             = False

Check if Nat number is smaller than another Nat number

> leN :: Nat -> Nat -> Bool
> leN a b
>   | ntoi (minusN b a) > 0 = True
>   | otherwise             = False

Check if Nat number is bigger than, or equal to another Nat number

> grEqN :: Nat -> Nat -> Bool
> grEqN a b = not (leN a b)

Check if Nat number is smaller than, or equal to another Nat number

> leEqN :: Nat -> Nat -> Bool
> leEqN a b = not (grN a b)

-----------
-- PART 2
-----------

> data Wochentag  = Montag | Dienstag | Mittwoch | Donnerstag | Freitag |
>                   Samstag | Sonntag 
>                   deriving (Eq,Show,Enum)
> type Tag        = Nat
> data Monat      = Jaenner | Feber | Maerz | April | Mai | Juni | Juli |
>                   August | September | Oktober | November | Dezember
>                   deriving (Eq,Show,Enum)
> type Jahr       = Nat
> type Datum      = (Tag,Monat,Jahr)
> type Schaltjahr = Bool

Convert days to integer

> daysi :: Wochentag -> Integer
> daysi w = toInteger (fromEnum w)

Convert integer to days

> idays :: Integer -> Wochentag
> idays w = toEnum (fromInteger w)

Convert month to integer

> monthi :: Monat -> Integer
> monthi m = toInteger (fromEnum m)

Convert integer to month

> imonth :: Integer -> Monat
> imonth m = toEnum (fromInteger m)

Sum up days till given date (in a year)

> sumDate :: Datum -> Schaltjahr -> Integer
> sumDate (d,m,j) sj
>  | not sj && 
>    monthi m > 1 = sum (take (fromInteger (monthi m)) days) + 
>                   (ntoi d) - 1
>  | otherwise    = sum (take (fromInteger (monthi m)) days) + 
>                   (ntoi d)
>  where
>    days = [31,29,31,30,31,30,31,31,30,31,30,31]

Sum up days till given date (all years)
    
> sumDateAll :: Datum -> Integer
> sumDateAll (d,m,j) = 366 * sjNum + 365 * (rj - sjNum) + y
>   where
>     ji    = ntoi j
>     rj    = ji - 1
>     sjNum = (div rj 4) - (div rj 100) + (div rj 400)
>     sj    = ((mod ji 4) == 0 && (mod ji 100) /= 0) ||
>             ((mod ji 4) == 0 && (mod ji 100) == 0 && 
>              (mod ji 400) == 0)
>     y     = sumDate (d,m,j) sj

> wochentag4 :: Datum -> Wochentag -> Datum -> Wochentag
> wochentag4 d1 day d2 = idays (mod ((daysi day) +
>                                    sumDateAll d2 -
>                                    sumDateAll d1) 7)
