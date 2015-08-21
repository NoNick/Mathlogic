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
              "(A->C)->(B->C)->(A|B->C)",
              "(A->B)->(A->!B)->!A",
              "!!A->A"]

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
    = foldl (\acc (b, n) -> if b then Just n else acc) Nothing (zip res [1..])
        where res = map ((flip evalState) HM.empty) states
              states = map ((flip match) e) axioms
              
-- first args is scheme, second is expression
-- no quantifiers in scheme!
match :: Expr -> Expr -> State (HM.Map String Expr) Bool
match (Forall _ _) _  = return False
match (Exists _ _) _  = return False
match (CustomP n p) e = do map <- get
                           let res = HM.findWithDefault err n map
                           if res == err then
                               (do modify (\m -> HM.insert n e m); return True)
                           else return $ e == res
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
    = fst $ runState (forall e e' S.empty) (x, Nothing)
matchForall _ = Right False

forallL :: [Expr] -> [Expr] -> S.Set String -> State (String, Maybe Expr) (Either String Bool)
forallL [] [] dom = return $ Right True
forallL e1 e2 dom
    = do res <- forall (head e1) (head e2) dom
         case res of
           (Right r) -> do res' <- forallL (tail e1) (tail e2) dom
                           case res' of
                             (Right r') -> return $ Right $ r && r'
                             (Left _)   -> return res'
           (Left _)  -> return res

-- first is scheme, second is expression, third is dom variables
forall :: Expr -> Expr -> S.Set String -> State (String, Maybe Expr) (Either String Bool)
forall e1@(Forall x e) e2@(Forall x' e') dom
    = do st <- get
         let ret = if x /= x' then return $ Right False
                   else forall e e' dom
         if x == (fst st) then
             return $ Right (e1 == e2)
         else if x /= x' then return $ Right False
              else forall e e' (S.insert x dom)
forall (Exists x e) (Exists x' e') dom
    = forall (Forall x e) (Forall x' e') dom
forall v@(Var x) e dom
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
forall (CustomP n e) (CustomP n' e') dom
    = if n == n' then forallL e e' dom
      else return $ Right False
forall e1 e2 dom = if same e1 e2 then
                       do let a1 = getArgs e1
                          let a2 = getArgs e2
                          forallL a1 a2 dom
                   else return $ Right False

-- True if expression matches scheme P[x:=y]->?xP and subst is OK
-- False if expression doesn't match the scheme
-- String error if substitute isn't OK
matchExists :: Expr -> Either String Bool
matchExists (Imply e (Exists x e'))= matchForall (Imply (Forall x e') e)
matchExists _ = Right False

