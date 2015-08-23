{-# LANGUAGE DeriveGeneric #-}

module Expression where

import qualified Data.HashMap as HM
import GHC.Generics (Generic)
import Data.Hashable
import Data.List

class SameOp a where
    same :: a -> a -> Bool

class Extractable a where
    getArgs :: a -> [a]

data Expr = Imply Expr Expr | Disj Expr Expr | Conj Expr Expr|
            Not Expr | Forall String Expr | Exists String Expr |
            EqualsP Expr Expr | CustomP String [Expr] |
            Sum [Expr] | Mult [Expr] | Var String | Zero |
            Inc Expr | Func String [Expr] deriving (Eq, Generic)
                
type Header = ([Expr], Expr)
type Proof = [Expr]

instance Hashable Expr
    
instance Ord Expr where
    (<) e1 e2 = (show e1) < (show e2)
    (>=) e1 e2 = (show e1) >= (show e2)
    (>) e1 e2 = (show e1) > (show e2)
    (<=) e1 e2 = (show e1) <= (show e2)
    max e1 e2 = if e1 > e2 then e1 else e2
    min e1 e2 = if e1 < e2 then e1 else e2

instance Show Expr where
    show (Sum t)       = case t of
                           (t':[]) -> show t'
                           _ -> "(" ++ (intercalate "+" $ map show t) ++ ")"
    show (Mult t)      = case t of
                           (t':[]) -> show t'
                           _ -> "(" ++ (intercalate "*" $ map show t) ++ ")"
    show (Var v)       = v
    show Zero          = "0"
    show (Inc t)       = "(" ++ (show t) ++ ")\'"
    show (Func n t)    = n ++ "(" ++ (intercalate "," $ map show t) ++ ")"
    show (EqualsP t1 t2) = "(" ++ (show t1) ++ "=" ++ (show t2) ++ ")"
    show (CustomP n [])  = n
    show (CustomP n t)   = n ++ "(" ++ (intercalate "," $ map show t) ++ ")"
    show (Imply e1 e2) = "(" ++ (show e1) ++ ")->(" ++ (show e2) ++ ")"
    show (Disj e1 e2)  = "(" ++ (show e1) ++ ")|(" ++ (show e2) ++ ")"
    show (Conj e1 e2)  = "(" ++ (show e1) ++ ")&(" ++ (show e2) ++ ")"
    show (Not e)       = "!(" ++ (show e) ++ ")"
    show (Forall v e)  = "@" ++ v ++ "(" ++ (show e) ++ ")"
    show (Exists v e)  = "?" ++ v ++ "(" ++ (show e) ++ ")"

instance SameOp Expr where
    same (Imply _ _) (Imply _ _)     = True
    same (Disj _ _)    (Disj _ _)    = True
    same (Conj _ _)    (Conj _ _)    = True
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
    getArgs (Forall _ e)    = [e]
    getArgs (Exists _ e)    = [e]
    getArgs (Imply e1 e2)   = [e1, e2]
    getArgs (Disj e1 e2)    = [e1, e2]
    getArgs (Conj e1 e2)    = [e1, e2]
    getArgs (Not e)         = [e]
    getArgs (Sum t)         = t
    getArgs (Mult t)        = t
    getArgs v@(Var _)       = [v]
    getArgs Zero            = []
    getArgs (Inc t)         = [t]
    getArgs (Func _ t)      = t
    getArgs (EqualsP t1 t2) = [t1, t2]
    getArgs (CustomP _ t)   = t

replace :: HM.Map String Expr -> Expr -> Expr
replace m (Forall x e)    = Forall x (replace m e)
replace m (Exists x e)    = Exists x (replace m e)
replace m (Imply e1 e2)   = Imply (replace m e1) (replace m e2)
replace m (Disj e1 e2)    = Disj (replace m e1) (replace m e2)
replace m (Conj e1 e2)    = Conj (replace m e1) (replace m e2)
replace m (Not e)         = Not $ replace m e
replace m (Sum t)         = Sum $ map (replace m) t
replace m (Mult t)        = Mult $ map (replace m) t
replace m v@(Var x)        = case HM.lookup x m of
                              (Just e) -> e
                              Nothing  -> v
replace m Zero            = Zero
replace m (Inc t)         = Inc $ replace m t
replace m (Func f t)      = Func f $ map (replace m) t
replace m (EqualsP t1 t2) = EqualsP (replace m t1) (replace m t2)
replace m c@(CustomP p [])= case HM.lookup p m of
                              (Just e) -> e
                              Nothing  -> c
replace m (CustomP p e)   = CustomP p $ map (replace m) e

-- grammar makes this expression illigal
err = CustomP "E" []
