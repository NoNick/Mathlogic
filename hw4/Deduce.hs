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

unpackE (Right r) = r
unpackE (Left _)  = False
    
unpackM (Just r) = r
unpackM Nothing  = S.empty

unpackL (Right r) = r
unpackL (Left _)  = []

fvAlpha :: Header -> S.Set String
fvAlpha = fv . last . fst

leftStr (Left s) = s
leftStr _ = ""

concatE :: [Either String Bool] -> String
concatE [] = ""
concatE (x:xs) = case x of
                   (Left l) -> l
                   (Right r) -> concatE xs

loadLemma :: String -> IO Proof
loadLemma path = do file <- B.readFile path
                    let p = P.parse pProof "" file
                    case p of
                      (Right r) -> return r
                      (Left _)  -> return []
            
reverseForall (Imply e1 (Forall x e)) vars
    = if S.member x (fv e1) || S.member x vars then err
      else Imply e1 e
reverseForall _ _ = err

reverseExists (Imply (Exists x e) e2) vars
    = if S.member x (fv e2) || S.member x vars then err
      else Imply e e2
reverseExists _ _ = err

-- if e = e1 -> e2 -> ... -> en
-- adds in map entries such as map[en] = e1 -> .. -> e(n-1)
--                                     ............
--                             map[e2 -> ... -> en] = e1
slice :: Expr -> HM.Map Expr (S.Set Expr) -> HM.Map Expr (S.Set Expr)
slice (Imply begin end) map = slice end (HM.insert end set map)
    where set = S.insert begin $ unpackM $ HM.lookup end map
slice _ map = map

-- dlist of modified proof; hm gives first part by second part of implication
-- set contains all processed expressions; list of proofes is lemmas
-- if proof is correct Nothing is returnted, otherwise error is returned
deduce :: Header -> Int -> [Proof] -> Proof -> WriterT (DL.DList Expr) (State (HM.Map Expr (S.Set Expr), S.Set Expr)) (Maybe String)
deduce h n l [] = return Nothing
deduce h n l (x:xs)
    = do (m, s) <- get
         modify (\(m, s) -> (slice x m, S.insert x s))
         
         -- MP: x == e_i, y == e_j -> e_i
         let parts = unpackM $ HM.lookup x m
         let e_j = S.intersection s parts
         -- G, a |- b; fv(a) shouldn't be used in predicate rules
         let ruleF = S.member (reverseForall x (fv $ last $ fst h)) s
         let ruleE = S.member (reverseExists x (fv $ last $ fst h)) s
         let ind = matchInd x
         let axiomF = matchForall x
         let axiomE = matchExists x
         let axiom = case matchAxiom x of
                       Just _ -> True
                       otherwise -> or $ map unpackE [ind, axiomF, axiomE]
                   
         if e_j /= S.empty then
             do let b = head $ S.toList e_j
                let m = HM.insert "A" (last $ fst h) $
                        HM.insert "B" b $
                        HM.insert "C" x HM.empty
                tell $ DL.fromList (map (replace m) (l !! 0))
                deduce h (succ n) l xs                     

         else if x == (last $ fst h) then
                  do let m = HM.insert "A" x HM.empty
                     tell $ DL.fromList (map (replace m) (l !! 1))
                     deduce h (succ n) l xs                   

         else if axiom then
                  do let m = HM.insert "B" x $
                             HM.insert "A" (last $ fst h) HM.empty
                     tell $ DL.fromList (map (replace m) (l !! 2))
                     deduce h (succ n) l xs                    

         else if ruleF || ruleE then
                  do case x of
                       (Imply b d@(Forall x c)) ->
                        do let m = HM.fromList [("A", (last $ fst h)), ("B", b), ("C", c), ("D", d)] 
                           tell $ DL.fromList (map (replace m) (l !! 3))
                       (Imply d@(Exists x b) c) -> 
                        do let m = HM.fromList [("A", (last $ fst h)), ("B", b), ("C", c), ("D", d)] 
                           tell $ DL.fromList (map (replace m) (l !! 4))
                     deduce h (succ n) l xs
                                    
         else return $ Just $ "Error in proof at line " ++ (show n) ++ (concatE [ind, axiomF, axiomE])


lemmas [] = []
lemmas (x:xs) = (unpackL $ P.parse pProof "" x):(lemmas xs)
                
main = do file <- B.readFile "input.txt"
          mp <- B.readFile "lemmas/mp"
          pr <- B.readFile "lemmas/premise"
          ax <- B.readFile "lemmas/axiom"
          fr <- B.readFile "lemmas/forallRule"
          er <- B.readFile "lemmas/existsRule"
          case P.parse pFile "" file of
            (Right (h, p)) ->
                (do let st = runState (runWriterT $ deduce h 1 (lemmas [mp, pr, ax, fr, er]) p) (HM.empty, S.empty)
                    let header = (intercalate "," $ map show (init $ fst h)) ++ "|-" ++ (show (Imply (last $ fst h) (snd h)))
                    let txt = (intercalate "\n" $ map show $ DL.toList $ snd $ fst st) ++ "\n"
                    case fst $ fst st of
                      Just err -> writeFile "output.txt" (err ++ "\n")
                      Nothing -> writeFile "output.txt" (header ++ "\n" ++ txt ++ "\n"))
            (Left e)       -> putStrLn $ show e
