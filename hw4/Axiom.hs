module Axiom where

import qualified Data.ByteString.Char8 as C
import Control.Monad.State
import qualified Data.HashMap as HM
import qualified Text.Parsec as P
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
match s e = if sameOp s e then
                matchList (getArgs s) (getArgs e)
            else return False

matchList :: [Expr] -> [Expr] -> State (HM.Map String Expr) Bool
matchList [] [] = return True
matchList s e = do res <- match (head s) (head e)
                   if res then matchList (tail s) (tail e)
                   else return False
