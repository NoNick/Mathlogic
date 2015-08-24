module Ordinal where

import CNF
    
data Ordinal = Sum Ordinal Ordinal | Mult Ordinal Ordinal |
               Pow Ordinal Ordinal | Num Integer | Omega

instance Show Ordinal where
    show (Sum o1 o2) = "(" ++ (show o1) ++ "+" ++ (show o2) ++ ")"
    show (Mult o1 o2) = "(" ++ (show o1) ++ "*" ++ (show o2) ++ ")"
    show (Pow o1 o2) = "(" ++ (show o1) ++ "^" ++ (show o2) ++ ")"
    show (Num n) = show n
    show Omega = "w"

ord2CNF :: Ordinal -> CNF
ord2CNF (Num n) = Atom n
ord2CNF Omega = List (Atom 1, 1) (Atom 0)
ord2CNF (Sum o1 o2) = add (ord2CNF o1) (ord2CNF o2)
ord2CNF (Mult o1 o2) = mul (ord2CNF o1) (ord2CNF o2)
ord2CNF (Pow o1 o2) = CNF.exp (ord2CNF o1) (ord2CNF o2)
