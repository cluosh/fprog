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
poundSterling (ps,_,_)  = ps

-- Check if amount of money is valid
isSound :: Amount -> Bool
isSound a
        | pence a >= 0 && shilling a >= 0 && poundSterling a >= 0 = True
        | otherwise                                               = False
        
-- Normalize the money
normalize :: Amount -> Amount
normalize a
        | isSound a     = (mod (pence a) 20, mod s 12, ps)
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

-- Interest rate
type InterestRate       = Float
type Years                      = Int
type Period                     = Years
interest :: Amount -> InterestRate -> Period -> Amount
interest a p j
        | isSound a && p >= 0 && j >= 1 = -- TODO
        | otherwise                                     = (0,0,0)
