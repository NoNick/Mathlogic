module Expression where

import Data.List
    
data Expr = Imply Expr Expr | Disj [Expr] | Conj [Expr] |
            Not Expr | Forall String Expr | Exists String Expr |
            BracketsE Expr | Predicate Pred deriving (Eq)
data Pred = Equals Term Term | Custom String [Term] deriving (Eq)
data Term = Sum [Term] | Mult [Term] | BracketsT Term |
            Var String | Zero | Inc Term | Func String [Term] deriving (Eq)

type Header = ([Expr], Expr)
type Proof = [Expr]

instance Show Term where
    show (Sum t)       = intercalate " + " $ map show t
    show (Mult t)      = "(" ++ (intercalate " * " $ map show t) ++ ")"
    show (BracketsT t) = "(" ++ (show t) ++ ")"
    show (Var v)       = v
    show Zero          = "0"
    show (Inc t)       = "(" ++ (show t) ++ ")\'"
    show (Func n t)    = n ++ "(" ++ (intercalate "," $ map show t) ++ ")"

instance Show Pred where
    show (Equals t1 t2) = (show t1) ++ "=" ++ (show t2)
    show (Custom n [])  = n
    show (Custom n t)   = n ++ "(" ++ (intercalate "," $ map show t) ++ ")"

instance Show Expr where
    show (Imply e1 e2) = "(" ++ (show e1) ++ ") -> (" ++ (show e2) ++ ")"
    show (Disj e)      = "(" ++ (intercalate "|" $ map show e) ++ ")"
    show (Conj e)      = "(" ++ (intercalate "&" $ map show e) ++ ")"
    show (Not e)       = "!" ++ (show e)
    show (Forall v e)  = "@" ++ v ++ "(" ++ (show e) ++ ")"
    show (Exists v e)  = "?" ++ v ++ "(" ++ (show e) ++ ")"
    show (BracketsE e) = "(" ++ (show e) ++ ")"
    show (Predicate p) = show p

sameOp :: Expr -> Expr -> Bool
sameOp (Imply _ _) (Imply _ _)     = True
sameOp (Disj e)    (Disj e')       = (length e) == (length e')
sameOp (Conj e)    (Conj e')       = (length e) == (length e')
sameOp (Not _)     (Not _)         = True
sameOp (Forall _ _) (Forall _ _)   = True
sameOp (Exists _ _) (Exists _ _)   = True
sameOp (BracketsE _) (BracketsE _) = True
sameOp (Predicate _) (Predicate _) = True
sameOp _ _                         = False

getName :: Pred -> String
getName (Custom n _) = n

getArgs :: Expr -> [Expr]
getArgs (Imply e1 e2) = [e1, e2]
getArgs (Disj e)      = e
getArgs (Conj e)      = e
getArgs (Not e)       = [e]
getArgs (BracketsE e) = [e]
