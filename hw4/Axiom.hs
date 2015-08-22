module Axiom where

import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap as HM
import qualified Text.Parsec as P
import qualified Data.Set as S
import Control.Monad.State
import Expression
import Parse

axiomsText = ["A->B->A",
              "(A->B)->(A->B->C)->(A->C)",
              "A->B->A&B",
              "A&B->A",
              "A&B->B",
              "A->A|B",
              "B->A|B",
              "(A->B)->(C->B)->A|C->B",
              "(A->B)->(A->!B)->!A",
              "!!A->A",
              "a=b->a'=b'",
              "a=b->a=c->b=c",
              "a'=b'->a=b",
              "!a'=0",
              "a+b'=(a+b)'",
              "a+0=a",
              "a*0=0",
              "a*b'=a*b+a"]

unpack (Right r) = r
unpack (Left _)  = err

fv :: Expr -> S.Set String
fv (Forall v e)  = S.delete v (fv e)
fv (Exists v e)  = S.delete v (fv e)
fv (Var v)       = S.singleton v
fv e             = S.unions $ map fv $ getArgs e

axioms :: [Expr]
axioms = map unpack $ map (P.parse pExpr "")
         (map C.pack axiomsText)

matchAxiom :: Expr -> Maybe Int
matchAxiom e
    = foldl (\acc (b, n) -> if b then Just n else acc) Nothing res'
        where res = map ((flip evalState) HM.empty) states
              res' = zip res [1..]
              states = map ((flip match) e) axioms
              
-- first args is scheme, second is expression
-- in sceme predicate with empty list of args stands for variable
--          any variable (term) stands for variable
match :: Expr -> Expr -> State (HM.Map String Expr) Bool
match (Var n) e
    = do map <- get
         let res = HM.findWithDefault err n map
         if res == err then
             (do modify (\m -> HM.insert n e m); return True)
         else return $ e == res
match (CustomP n p) e = match (Var n) e
match s e = if same s e then
                matchList (getArgs s) (getArgs e)
            else return False

matchList :: [Expr] -> [Expr] -> State (HM.Map String Expr) Bool
matchList [] [] = return True
matchList s e = do res <- match (head s) (head e)
                   if res then matchList (tail s) (tail e)
                   else return False

-- True if expression matches scheme @xP->P[x:=y] and subst is OK
-- False if expression doesn't match the scheme
-- String error if substitute isn't OK
matchForall :: Expr -> Either String Bool
matchForall (Imply (Forall x e) e')
    = fst $ runState (subst e e' S.empty) (x, Nothing)
matchForall _ = Right False

substL :: [Expr] -> [Expr] -> S.Set String -> State (String, Maybe Expr) (Either String Bool)
substL [] [] dom = return $ Right True
substL e1 e2 dom
    = do res <- subst (head e1) (head e2) dom
         case res of
           (Right r) -> do res' <- substL (tail e1) (tail e2) dom
                           case res' of
                             (Right r') -> return $ Right $ r && r'
                             (Left _)   -> return res'
           (Left _)  -> return res

-- state: x=e; args: e1 e2 dom
-- if e is Nothing, finds it; returns false if e is ambiguous
-- returns true if substitution e1[x:=e]=e2 is OK, false otherwise
-- returns String if a substituion rule violated
subst :: Expr -> Expr -> S.Set String -> State (String, Maybe Expr) (Either String Bool)
subst e1@(Forall x e) e2@(Forall x' e') dom
    = do st <- get
         -- other dom variable with same name means
         -- everything under quantifier remains unchanged
         if x == (fst st) then 
             return $ Right (e1 == e2)
         else if x /= x' then return $ Right False
              else subst e e' (S.insert x dom)
subst (Exists x e) (Exists x' e') dom
    = subst (Forall x e) (Forall x' e') dom
subst v@(Var x) e dom
    = do st <- get
         let res = if S.null $ S.intersection dom (fv e) then
                       return $ Right True
                   else return $ Left ("Free variable in " ++ (show e)
                                        ++ " becomes dom variable.")
         if x == (fst st) then
             case snd st of           
               Just e' -> if e == e' then res
                          else return $ Left ("Substitution of variable " ++ x ++ " with two or more terms.")
               Nothing -> do put (x, Just e)
                             res
         else return $ Right (v == e)
subst (CustomP n e) (CustomP n' e') dom
    = if n == n' then substL e e' dom
      else return $ Right False
subst e1 e2 dom = if same e1 e2 then
                      do let a1 = getArgs e1
                         let a2 = getArgs e2
                         substL a1 a2 dom
                  else return $ Right False

-- True if expression matches scheme P[x:=y]->?xP and subst is OK
-- False if expression doesn't match the scheme
-- String error if substitute isn't OK
matchExists :: Expr -> Either String Bool
matchExists (Imply e (Exists x e'))
    = fst $ runState (subst e' e S.empty) (x, Nothing)
matchExists _ = Right False

-- True if expression matches mathematic induction axiom, false otherwise
-- Returns String err if substition isn't OK
matchInd :: Expr -> Either String Bool
matchInd (Imply a e)
    = case a of
        (Conj e1 f) -> case f of
                         (Forall x i) -> case i of
                                           (Imply e' e2) -> matchInd' x e e' e1 e2
                                           otherwise -> Right False
                         otherwise -> Right False
        otherwise -> Right False
matchInd _ = Right False

-- e1&@x(e'->e2)->e
matchInd' x e e' e1 e2
    = case s1 of
        (Right r) -> case s2 of
                       (Right r') -> Right $ r && r' && (e == e')
                       l@(Left _) -> l
        l@(Left _) -> l
        where s1 = fst $ runState (subst e e1 S.empty) (x, Just $ Zero)
              s2 = fst $ runState (subst e e2 S.empty) (x, Just $ Inc $ Var x)

--Disj (Disj (a=2) (a=1)) (a=0)
