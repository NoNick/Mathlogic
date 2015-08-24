-- http://www.ccs.neu.edu/home/pete/pub/cade-algorithms-ordinal-arithmetic.pdf
module CNF where

data CNF = List (CNF, Integer) CNF |
           Atom Integer | Nil deriving (Eq)

instance Show CNF where
    show (List (c, n) c') = (show n) ++ "*w^(" ++ (show c) ++ ") + " ++ (show c')
    show (Atom n) = show n
    show Nil = "nil"
               
atom (Atom _) = True
atom _ = False
               
first Nil = (Nil, 0)
first (Atom a) = (Atom a, 1)
first (List head _) = head

rest Nil = Nil
rest (Atom _) = Nil
rest (List _ tail) = tail

firstn a 0 = Nil
firstn a@(Atom _) n = a
firstn (List x xs) n = List x (firstn xs (n - 1))

restn a 0 = a
restn a@(Atom _) n = Nil
restn (List x xs) n = restn xs (n - 1)

fe (Atom _) = Atom 0
fe a = fst $ first a

fc (Atom a) = a
fc a = snd $ first a

len Nil = 0
len (Atom _) = 0
len a = (len $ rest a) + 1

size Nil = 0
size (Atom _) = 1
size a = (size $ fe a) + (size $ rest a)

append Nil b = b
append a Nil = a
append (Atom a) b = b
append a b = List (first a) (append (rest a) b)

cmp Nil Nil = EQ
cmp Nil b = LT
cmp a Nil = GT
cmp a (List (_, 0) bs) = cmp a bs
cmp (List (_, 0) as) b = cmp as b
cmp (Atom a) (Atom b) = compare a b
cmp (Atom a) _ = LT
cmp _ (Atom b) = GT
cmp l1@(List (a1, a2) as) l2@(List (b1, b2) bs)
    | (cmp a1 b1) /= EQ = cmp a1 b1
    | a2 /= b2 = compare a2 b2
    | otherwise = cmp as bs

add Nil b = b
add a Nil = a
add (Atom a) (Atom b) = Atom (a + b)
add a b
    | cmp (fe a) (fe b) == LT = b
    | cmp (fe a) (fe b) == EQ = List (fe a, (fc a) + (fc b)) (rest b)
    | otherwise = List (fe a, fc a) (add (rest a) b)

sub (Atom a) (Atom b)
    | a < b = Atom 0
    | otherwise = Atom $ a - b
sub a b
    | cmp (fe a) (fe b) == LT = Atom 0
    | cmp (fe a) (fe b) == GT = a
    | (fc a) < (fc b) = Atom 0
    | (fc a) > (fc b) = List (fe a, (fc a) - (fc b)) (rest a)
    | otherwise = sub (rest a) (rest b)
                  
c a b
  | cmp (fe b) (fe a) == LT = 1 + (c (rest a) b)
  | otherwise = 0

count a b n = n + (c (restn a n) b)

padd a b n = append (firstn a n) (add (restn a n) b)

pmult Nil Nil n = Atom 0
pmult (Atom a) (Atom b) n = Atom (a * b)
pmult a (Atom b) n = List (fe a, (fc a) * b) (rest a)
pmult a b n
    = List (padd (fe a) (fe b) m,fc b) (pmult a (rest b) m)
                        where m = count (fe a) (fe b) n

mul a b = pmult a b 0

rest' (List _ (Atom n)) = n
          
exp1 p b
     | cmp (fe b) (Atom 1) == EQ
         = List (Atom $ fc b, p ^ (rest' b)) (Atom 0)
     | atom (rest b)
         = let e = List (sub (fe b) (Atom 1), fc b) (Atom 0) in
               List (e, p ^ (rest' b)) (Atom 0)
     | otherwise = let c = exp1 p (rest b) in
                       List (List (sub (fe b) (Atom 1), 1) (fe c), fc c) (Atom 0)

exp2 a 1 = a
exp2 a q = mul (List (mul (fe a) (Atom $ q - 1), 1) (Atom 0)) a

limitp (Atom a) = (a == 0)
limitp a = limitp $ rest a

limitpart (Atom a) = Atom 0
limitpart a = List (first a) (limitpart $ rest a)

natpart (Atom a) = a
natpart a = natpart $ rest a

helper a p n 0 = Atom p
helper a p n q = padd (mul (exp2 a q) (Atom p)) (helper a p n (q - 1)) n

exp3 a 0 = Atom 1
exp3 a 1 = a
exp3 a q
    | limitp a = exp2 a q
    | otherwise = let c = limitpart a in let n = len a in
                  padd (exp2 c q) (helper c (natpart a) n (q - 1)) n
                         
exp4 a b = mul (List (mul (fe a) (limitpart b), 1) (Atom 0)) (exp3 a (natpart b))

exp (Atom 1) _ = Atom 1
exp _ (Atom 0) = Atom 1
exp (Atom 0) _ = Atom 0
exp (Atom a) (Atom b) = Atom $ a ^ b
exp (Atom a) b = exp1 a b
exp a (Atom b) = exp3 a b
exp a b = exp4 a b
