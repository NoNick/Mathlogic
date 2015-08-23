module Ordinal where

data Ordinal = Sum Ordinal Ordinal | Mult Ordinal Ordinal |
               Pow Ordinal Ordinal | Num Int | Omega

instance Show Ordinal where
    show (Sum o1 o2) = "(" ++ (show o1) ++ "+" ++ (show o2) ++ ")"
    show (Mult o1 o2) = "(" ++ (show o1) ++ "*" ++ (show o2) ++ ")"
    show (Pow o1 o2) = "(" ++ (show o1) ++ "^" ++ (show o2) ++ ")"
    show (Num n) = show n
    show Omega = "w"
     
