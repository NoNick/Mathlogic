module Axiom where

import qualified Data.ByteString.Char8 as C
import Control.Monad.State
import qualified Data.HashMap as HM
import qualified Text.Parsec as P
import qualified Data.Set as S
import Expression
import Parse

axioms = ["A->B->A",
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
unpack (Left _)  = Predicate (Custom "E" [])

fvSt :: Expr -> State (S.Set String) ()
fvSt (Forall v e)  = fvSt e >> modify (S.delete v)
fvSt (Exists v e)  = fvSt e >> modify (S.delete v)
fvSt (Predicate p) = case p of
                     (Equals t1 t2) -> fvSt' t1 >> fvSt' t2
                     (Custom _ t) -> mapM_ fvSt' t
fvSt e             = mapM_ fvSt $ getArgs e
fvSt' :: Term -> State (S.Set String) ()
fvSt' (Var v) = modify (S.insert v)
fvSt' t       = mapM_ fvSt' $ getArgs t

fv :: Expr -> S.Set String
fv e  = snd $ runState (fvSt e) S.empty
fv':: Term -> S.Set String
fv' t = snd $ runState (fvSt' t) S.empty

getAxioms :: [Expr]
getAxioms = map unpack $ map (P.parse pExpr "")
              (map C.pack axioms)

def :: Expr
def = Predicate $ Custom "0" []

-- first args is scheme, second is expression
-- no quantifiers in scheme!
match :: Expr -> Expr -> State (HM.Map String Expr) Bool
match (Forall _ _) _ = return False
match (Exists _ _) _ = return False
match (Predicate p) e = do map <- get
                           let n = getName p
                           let res = HM.findWithDefault def n map
                           if res == def then
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
matchForall (Imply e1 e2)
    = case e1 of
        (Forall x e) -> let eval = fst $ runState (forall e e2 S.empty) (x, Nothing) in case e2 of
                                                                                  (Forall y _) -> if x == y then Right False
                                                                                                  else eval
                                                                                  otherwise -> eval  
        otherwise -> Right False
matchForall _ = Right False

forallL :: [Expr] -> [Expr] -> S.Set String -> State (String, Maybe Term) (Either String Bool)
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
forall :: Expr -> Expr -> S.Set String -> State (String, Maybe Term) (Either String Bool)
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
forall (Predicate p) (Predicate p') dom
    = if same p p' then forallL' (getArgs' p) (getArgs' p') dom
      else return $ Right False
forall (BracketsE e) e' dom = forall e e' dom
forall e (BracketsE e') dom = forall e e' dom
forall e1 e2 dom = if same e1 e2 then
                       do let a1 = getArgs e1
                          let a2 = getArgs e2
                          forallL a1 a2 dom
                   else return $ Right False

forallL' :: [Term] -> [Term] -> S.Set String -> State (String, Maybe Term) (Either String Bool)
forallL' [] [] dom = return $ Right True
forallL' e e' dom =
    do res <- forall' (head e) (head e') dom
       case res of
           (Right r) -> do res' <- forallL' (tail e) (tail e') dom
                           case res' of
                             (Right r') -> return $ Right $ r && r'
                             (Left _)   -> return res'
           (Left _)  -> return res

forall' :: Term -> Term -> S.Set String -> State (String, Maybe Term) (Either String Bool)
forall' v@(Var x) e dom
    = do st <- get
         let res = if S.null $ S.intersection dom (fv' e) then
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
forall' (BracketsT e) e' dom = forall' e e' dom
forall' e (BracketsT e') dom = forall' e e' dom          
forall' t1 t2 dom = if same t1 t2 then
                        do let a1 = getArgs t1
                           let a2 = getArgs t2
                           forallL' a1 a2 dom
                    else return $ Right False

