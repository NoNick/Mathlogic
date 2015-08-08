module Expression where

import Data.List

class SameOp a where
    same :: a -> a -> Bool

class Extractable a where
    getArgs :: a -> [a]

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

instance SameOp Expr where
    same (Imply _ _) (Imply _ _)     = True
    same (Disj e)    (Disj e')       = (length e) == (length e')
    same (Conj e)    (Conj e')       = (length e) == (length e')
    same (Not _)     (Not _)         = True
    same (Forall _ _) (Forall _ _)   = True
    same (Exists _ _) (Exists _ _)   = True
    same (BracketsE _) (BracketsE _) = True
    same (Predicate _) (Predicate _) = True
    same _ _                         = False

instance SameOp Term where
    same (Sum t) (Sum t') = (length t) == (length t')
    same (Mult t) (Mult t') = (length t) == (length t')
    same (BracketsT _) (BracketsT _) = True
    same (Var _) (Var _) = True
    same Zero Zero = True
    same (Inc _) (Inc _) = True
    same (Func f t) (Func f' t') =
        (f == f') && ((length t) == (length t'))

instance SameOp Pred where
    same (Equals _ _) (Equals _ _) = True
    same (Custom p t) (Custom p' t')
        = (p == p') && ((length t) == (length t'))
    same _ _ = False

getName :: Pred -> String
getName (Custom n _) = n

instance Extractable Expr where
    getArgs (Imply e1 e2) = [e1, e2]
    getArgs (Disj e)      = e
    getArgs (Conj e)      = e
    getArgs (Not e)       = [e]
    getArgs (BracketsE e) = [e]

instance Extractable Term where
    getArgs (Sum t)       = t
    getArgs (Mult t)      = t
    getArgs (BracketsT t) = [t]
    getArgs v@(Var _)     = [v]
    getArgs Zero          = []
    getArgs (Inc t)       = [t]
    getArgs (Func _ t)    = t

getArgs' (Equals t1 t2) = [t1, t2]
getArgs' (Custom _ t)   = t
