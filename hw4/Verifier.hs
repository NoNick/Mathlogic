module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S
import qualified Data.DList as DL
import qualified Data.HashMap as HM
import qualified Text.Parsec as P
import Control.Monad.Writer
import Control.Monad.State
import Expression
import Parse
import Axiom
import Data.List

unpackM (Just r) = r
unpackM Nothing  = S.empty

fvAlpha :: Header -> S.Set String
fvAlpha = fv . last . fst

leftStr (Left s) = s
leftStr _ = ""
          
reverseRule (Imply e1 e2) vars
    = case e2 of
        (Forall x e) -> if S.member x (fv e1) || S.member x vars then err
                        else Imply e1 e
        otherwise    -> case e1 of
                          (Exists x e) -> if S.member x (fv e2) || S.member x vars then err
                                          else Imply e e2
                          otherwise    -> err
reverseRule _ _ = err

-- if e = e1 -> e2 -> ... -> en
-- adds in map entries such as map[en] = e1 -> .. -> e(n-1)
--                                     ............
--                             map[e2 -> ... -> en] = e1
slice :: Expr -> HM.Map Expr (S.Set Expr) -> HM.Map Expr (S.Set Expr)
slice (Imply begin end) map = slice end (HM.insert end set map)
    where set = S.insert begin $ unpackM $ HM.lookup end map
slice _ map = map
                  
-- dlist of modified proof; hm gives first part by second part of implication
-- set contains all processed expressions
-- if proof is correct Nothing is returnted, otherwise error is returned
deduce :: Header -> Int-> Proof -> WriterT (DL.DList Expr) (State (HM.Map Expr (S.Set Expr), S.Set Expr)) (Maybe String)
deduce h n [] = return Nothing
deduce h n (x:xs)
    = do (m, s) <- get
         modify (\(m, s) -> (slice x m, S.insert x s))
         let parts = unpackM $ HM.lookup x m
         let mp = or $ S.toList $ S.map ((flip S.member) s) parts
         -- G, a |- b; fv(a) shouldn't be used in predicate rules
         let predRule = S.member (reverseRule x (fv $ last $ fst h)) s
         let axiom = case matchAxiom x of
                       Just _ -> True
                       otherwise -> False
         let fAxiom = matchForall x
         let eAxiom = matchExists x
         let premise = elem x $ fst h
         let ind = matchInd x
         case (fAxiom, eAxiom, ind) of
           (Right forall, Right exists, Right induction)
               -> if mp || predRule || axiom || forall || exists || premise || induction then
                      deduce h (succ n) xs
                  else return $ Just $ "Line  " ++ (show n) ++ " isn't an axiom or rule.\n"
           (a1, a2, a3) -> return $ Just $ concatMap leftStr [a1, a2, a3]                         

ex  = unpack $ P.parse pExpr "" $ B.pack "@xP(x)->P(x)"
ex' = unpack $ P.parse pExpr "" $ B.pack "(@xP(x)->P(x))->(@xP(x)->P(x))->!P(x)->@xP(x)->P(x)"

main = do file <- B.readFile "input.txt"
          case P.parse pFile "" file of
            (Right (h, p)) -> (do let err = runState (runWriterT $ deduce h 1 p) (HM.empty, S.empty)
                                  let d = fst $ fst err
                                  writeFile "output.txt" $ (show d) ++ "\n")
--                                  let hText = (intercalate ", " $ map show (fst h)) ++ " |- " ++ show (snd h)
--                                  let pText = (intercalate "\n" $ map show p)
--                                  writeFile "output.txt" (hText ++ "\n" ++ pText ++ "\n"))
            (Left e)       -> putStrLn $ show e
