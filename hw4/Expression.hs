{-# LANGUAGE DeriveGeneric #-}

module Expression where

import GHC.Generics (Generic)
import Data.Hashable
import Data.List

class SameOp a where
    same :: a -> a -> Bool

class Extractable a where
    getArgs :: a -> [a]

data Expr = Imply Expr Expr | Disj [Expr] | Conj [Expr] |
            Not Expr | Forall String Expr | Exists String Expr |
            EqualsP Expr Expr | CustomP String [Expr] |
            Sum [Expr] | Mult [Expr] | Var String | Zero |
            Inc Expr | Func String [Expr] deriving (Eq, Generic)
                
type Header = ([Expr], Expr)
type Proof = [Expr]

instance Hashable Expr

instance Ord Expr where
    (<) e1 e2 = (hash e1) < (hash e2)
    (>=) e1 e2 = (hash e1) >= (hash e2)
    (>) e1 e2 = (hash e1)  > (hash e2)
    (<=) e1 e2 = (hash e1) <= (hash e2)
    max e1 e2 = if e1 > e2 then e1 else e2
    min e1 e2 = if e1 < e2 then e1 else e2

instance Show Expr where
    show (Sum t)       = intercalate " + " $ map show t
    show (Mult t)      = "(" ++ (intercalate " * " $ map show t) ++ ")"
    show (Var v)       = v
    show Zero          = "0"
    show (Inc t)       = "(" ++ (show t) ++ ")\'"
    show (Func n t)    = n ++ "(" ++ (intercalate "," $ map show t) ++ ")"
    show (EqualsP t1 t2) = (show t1) ++ "=" ++ (show t2)
    show (CustomP n [])  = n
    show (CustomP n t)   = n ++ "(" ++ (intercalate "," $ map show t) ++ ")"
    show (Imply e1 e2) = "(" ++ (show e1) ++ ") -> (" ++ (show e2) ++ ")"
    show (Disj e)      = "(" ++ (intercalate "|" $ map show e) ++ ")"
    show (Conj e)      = "(" ++ (intercalate "&" $ map show e) ++ ")"
    show (Not e)       = "!" ++ (show e)
    show (Forall v e)  = "@" ++ v ++ "(" ++ (show e) ++ ")"
    show (Exists v e)  = "?" ++ v ++ "(" ++ (show e) ++ ")"

instance SameOp Expr where
    same (Imply _ _) (Imply _ _)     = True
    same (Disj e)    (Disj e')       = (length e) == (length e')
    same (Conj e)    (Conj e')       = (length e) == (length e')
    same (Not _)     (Not _)         = True
    same (Forall _ _) (Forall _ _)   = True
    same (Exists _ _) (Exists _ _)   = True
    same (Sum t) (Sum t') = (length t) == (length t')
    same (Mult t) (Mult t') = (length t) == (length t')
    same (Var _) (Var _) = True
    same Zero Zero = True
    same (Inc _) (Inc _) = True
    same (Func f t) (Func f' t') =
        (f == f') && ((length t) == (length t'))
    same (EqualsP _ _) (EqualsP _ _) = True
    same (CustomP p t) (CustomP p' t')
        = (p == p') && ((length t) == (length t'))
    same _ _ = False

getName :: Expr -> String
getName (CustomP n _) = n

instance Extractable Expr where
    getArgs (Imply e1 e2)   = [e1, e2]
    getArgs (Disj e)        = e
    getArgs (Conj e)        = e
    getArgs (Not e)         = [e]
    getArgs (Sum t)         = t
    getArgs (Mult t)        = t
    getArgs v@(Var _)       = [v]
    getArgs Zero            = []
    getArgs (Inc t)         = [t]
    getArgs (Func _ t)      = t
    getArgs (EqualsP t1 t2) = [t1, t2]
    getArgs (CustomP _ t)   = t

-- grammar makes this expression impossible
err = CustomP "E" []
